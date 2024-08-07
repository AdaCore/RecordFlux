from __future__ import annotations

import typing as ty
from pathlib import Path
from typing import Optional

from annotated_types import Gt
from pydantic import BaseModel, ConfigDict, Field, ValidationError
from ruamel.yaml.error import MarkedYAMLError
from ruamel.yaml.main import YAML
from typing_extensions import Annotated

from rflx.identifier import ID
from rflx.model import Model
from rflx.model.session import Session
from rflx.rapidflux import ErrorEntry, Location, RecordFluxError, Severity

# TODO(eng/recordflux/RecordFlux#1359): Replace ty.* by collections.abc.*
# Sequence and Mapping are imported from collections.abc as importing them
# from typing is deprecated. However pydantic does not support the imported
# version from collections.abc. To fix that typing is imported as ty and the
# typing versions of Sequence and Mapping are used in classes that derive
# from pydantic.BaseModel.
# This is only relevant for Python 3.8.


IntSize = Annotated[int, Gt(0)]


class SessionSize(BaseModel):  # type: ignore[misc]
    default: Optional[IntSize] = Field(None, alias="Default")
    global_: Optional[ty.Mapping[str, IntSize]] = Field(None, alias="Global")
    local_: Optional[ty.Mapping[str, ty.Mapping[str, IntSize]]] = Field(None, alias="Local")

    model_config = ConfigDict(extra="forbid")


class SessionIntegration(BaseModel):  # type: ignore[misc]
    buffer_size: Optional[SessionSize] = Field(alias="Buffer_Size", default=None)
    external_io_buffers: bool = Field(alias="External_IO_Buffers", default=False)

    model_config = ConfigDict(extra="forbid")


class IntegrationFile(BaseModel):  # type: ignore[misc]
    session: ty.Mapping[str, SessionIntegration] = Field(alias="Session")

    model_config = ConfigDict(extra="forbid")


class Integration:
    @property
    def defaultsize(self) -> int:
        return 4096

    def __init__(self, integration_files_dir: Optional[Path] = None) -> None:
        self._packages: dict[str, IntegrationFile] = {}
        self._integration_files_dir = integration_files_dir

    def load_integration_file(self, spec_file: Path, error: RecordFluxError) -> None:
        integration_file = (
            spec_file.with_suffix(".rfi")
            if self._integration_files_dir is None
            else self._integration_files_dir / (spec_file.stem + ".rfi")
        )
        if integration_file.exists():
            yaml = YAML()
            try:
                content = yaml.load(integration_file)
            except MarkedYAMLError as e:
                location = Location(
                    start=(
                        (1, 1)
                        if e.problem_mark is None
                        else (e.problem_mark.line + 1, e.problem_mark.column + 1)
                    ),
                    source=integration_file,
                )
                error.push(ErrorEntry(str(e), Severity.ERROR, location))
                return
            self._add_integration_object(integration_file, content, error)

    def validate(self, model: Model, error: RecordFluxError) -> None:
        for package, integration_file in self._packages.items():
            for session_name, integration in integration_file.session.items():
                matching_sessions = [
                    s
                    for s in model.sessions
                    if package == str(s.package).lower() and str(s.identifier.name) == session_name
                ]
                if not matching_sessions:
                    error.push(
                        ErrorEntry(
                            f'unknown session "{session_name}"',
                            Severity.ERROR,
                            Integration._to_location(package),
                        ),
                    )
                    return
                assert len(matching_sessions) == 1
                session = matching_sessions[0]
                self._validate_globals(package, integration, session, error)
                self._validate_states(package, integration, session, error)

    def get_size(self, session: ID, variable: Optional[ID], state: Optional[ID]) -> int:
        """
        Return the requested buffer size for a variable of a given session and state.

        If state is None, the variable is assumed to be a global variable. If variable is None or no
        specific buffer size was requested for the variable, return the default buffer size for the
        session, if present, or the default buffer size for RecordFlux.

        The returned size is in bytes.
        """
        integration_package = str(session.parent).lower()
        if integration_package not in self._packages:
            return self.defaultsize

        session_name = str(session.name)
        if session_name not in self._packages[integration_package].session:
            return self.defaultsize

        buffer_size = self._packages[integration_package].session[session_name].buffer_size
        if buffer_size is None:
            return self.defaultsize

        default_size = self.defaultsize if buffer_size.default is None else buffer_size.default

        if variable is None:
            return default_size

        variable_name = str(variable.name)
        if state is None:
            if buffer_size.global_ is not None and variable_name in buffer_size.global_:
                return buffer_size.global_[variable_name]
            return default_size

        state_name = str(state)
        if (
            buffer_size.local_ is not None
            and state_name in buffer_size.local_
            and variable_name in buffer_size.local_[state_name]
        ):
            return buffer_size.local_[state_name][variable_name]

        if buffer_size.global_ is not None and variable_name in buffer_size.global_:
            return buffer_size.global_[variable_name]

        return default_size

    def use_external_io_buffers(self, session: ID) -> bool:
        integration_package = str(session.parent).lower()
        if integration_package not in self._packages:
            return False

        session_name = str(session.name)
        if session_name not in self._packages[integration_package].session:
            return False

        return self._packages[integration_package].session[session_name].external_io_buffers

    def add_integration_file(self, package_name: str, integration_file: IntegrationFile) -> None:
        self._packages[package_name] = integration_file

    def _add_integration_object(
        self,
        filename: Path,
        content: object,
        error: RecordFluxError,
    ) -> None:
        try:
            self.add_integration_file(filename.stem, IntegrationFile.model_validate(content))
        except ValidationError as e:
            error.push(ErrorEntry(f"{e}", Severity.ERROR, self._to_location(filename.stem)))

    @staticmethod
    def _to_location(package: str) -> Location:
        return Location(start=(1, 1), source=Path(package + ".rfi"))

    @staticmethod
    def _validate_globals(
        package: str,
        integration: SessionIntegration,
        session: Session,
        error: RecordFluxError,
    ) -> None:
        if integration.buffer_size is None or integration.buffer_size.global_ is None:
            return
        session_decl_vars = [str(x.name) for x in session.declarations]
        for var_name in integration.buffer_size.global_:
            if var_name not in session_decl_vars:
                error.push(
                    ErrorEntry(
                        (
                            f'unknown global variable "{var_name}" '
                            f'in session "{session.identifier.name}"'
                        ),
                        Severity.ERROR,
                        Integration._to_location(package),
                    ),
                )

    @staticmethod
    def _validate_states(
        package: str,
        integration: SessionIntegration,
        session: Session,
        error: RecordFluxError,
    ) -> None:
        if integration.buffer_size is None or integration.buffer_size.local_ is None:
            return
        for state_name, state_entry in integration.buffer_size.local_.items():
            state = None
            for s in session.states:
                if str(s.identifier.name) == state_name:
                    state = s
            if state is None:
                error.push(
                    ErrorEntry(
                        (
                            f'unknown state "{state_name}" in session '
                            f'"{session.identifier.name}"'
                        ),
                        Severity.ERROR,
                        Integration._to_location(package),
                    ),
                )
                return
            state_declaration_vars = [str(x.name) for x in state.declarations]
            for var_name in state_entry:
                if var_name not in state_declaration_vars:
                    error.push(
                        ErrorEntry(
                            (
                                f'unknown variable "{var_name}" in state '
                                f'"{state_name}" of session "{session.identifier.name}"'
                            ),
                            Severity.ERROR,
                            Integration._to_location(package),
                        ),
                    )
