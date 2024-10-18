from __future__ import annotations

from collections.abc import Mapping
from pathlib import Path
from typing import Annotated, Optional

from annotated_types import Gt
from pydantic import BaseModel, ConfigDict, Field, ValidationError
from ruamel.yaml.error import MarkedYAMLError
from ruamel.yaml.main import YAML

from rflx.identifier import ID
from rflx.model import Model
from rflx.model.state_machine import StateMachine
from rflx.rapidflux import ErrorEntry, Location, RecordFluxError, Severity

# TODO(eng/recordflux/RecordFlux#1424): Replace remaining use of Optional
# and Union. Pydantic has issues with PEP604 type annotations in Python 3.9.

IntSize = Annotated[int, Gt(0)]


class StateMachineSize(BaseModel):  # type: ignore[misc]
    default: Optional[IntSize] = Field(None, alias="Default")  # noqa: UP007
    global_: Optional[Mapping[str, IntSize]] = Field(None, alias="Global")  # noqa: UP007
    local_: Optional[Mapping[str, Mapping[str, IntSize]]] = Field(  # noqa: UP007
        None,
        alias="Local",
    )

    model_config = ConfigDict(extra="forbid")


class StateMachineIntegration(BaseModel):  # type: ignore[misc]
    buffer_size: Optional[StateMachineSize] = Field(  # noqa: UP007
        alias="Buffer_Size",
        default=None,
    )
    external_io_buffers: bool = Field(alias="External_IO_Buffers", default=False)

    model_config = ConfigDict(extra="forbid")


class IntegrationFile(BaseModel):  # type: ignore[misc]
    state_machine: Mapping[str, StateMachineIntegration] = Field(alias="Machine")

    model_config = ConfigDict(extra="forbid")


class Integration:
    @property
    def defaultsize(self) -> int:
        return 4096

    def __init__(self, integration_files_dir: Path | None = None) -> None:
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
            for state_machine_name, integration in integration_file.state_machine.items():
                matching_state_machines = [
                    s
                    for s in model.state_machines
                    if package == str(s.package).lower()
                    and str(s.identifier.name) == state_machine_name
                ]
                if not matching_state_machines:
                    error.push(
                        ErrorEntry(
                            f'unknown state machine "{state_machine_name}"',
                            Severity.ERROR,
                            Integration._to_location(package),
                        ),
                    )
                    return
                assert len(matching_state_machines) == 1
                state_machine = matching_state_machines[0]
                self._validate_globals(package, integration, state_machine, error)
                self._validate_states(package, integration, state_machine, error)

    def get_size(self, state_machine: ID, variable: ID | None, state: ID | None) -> int:
        """
        Return the requested buffer size for a variable of a given state machine and state.

        If state is None, the variable is assumed to be a global variable. If variable is None or no
        specific buffer size was requested for the variable, return the default buffer size for the
        state machine, if present, or the default buffer size for RecordFlux.

        The returned size is in bytes.
        """
        integration_package = str(state_machine.parent).lower()
        if integration_package not in self._packages:
            return self.defaultsize

        state_machine_name = str(state_machine.name)
        if state_machine_name not in self._packages[integration_package].state_machine:
            return self.defaultsize

        buffer_size = (
            self._packages[integration_package].state_machine[state_machine_name].buffer_size
        )
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

    def use_external_io_buffers(self, state_machine: ID) -> bool:
        integration_package = str(state_machine.parent).lower()
        if integration_package not in self._packages:
            return False

        state_machine_name = str(state_machine.name)
        if state_machine_name not in self._packages[integration_package].state_machine:
            return False

        return (
            self._packages[integration_package]
            .state_machine[state_machine_name]
            .external_io_buffers
        )

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
        integration: StateMachineIntegration,
        state_machine: StateMachine,
        error: RecordFluxError,
    ) -> None:
        if integration.buffer_size is None or integration.buffer_size.global_ is None:
            return
        state_machine_decl_vars = [str(x.name) for x in state_machine.declarations]
        for var_name in integration.buffer_size.global_:
            if var_name not in state_machine_decl_vars:
                error.push(
                    ErrorEntry(
                        (
                            f'unknown global variable "{var_name}" '
                            f'in state machine "{state_machine.identifier.name}"'
                        ),
                        Severity.ERROR,
                        Integration._to_location(package),
                    ),
                )

    @staticmethod
    def _validate_states(
        package: str,
        integration: StateMachineIntegration,
        state_machine: StateMachine,
        error: RecordFluxError,
    ) -> None:
        if integration.buffer_size is None or integration.buffer_size.local_ is None:
            return
        for state_name, state_entry in integration.buffer_size.local_.items():
            state = None
            for s in state_machine.states:
                if str(s.identifier.name) == state_name:
                    state = s
            if state is None:
                error.push(
                    ErrorEntry(
                        (
                            f'unknown state "{state_name}" in state machine '
                            f'"{state_machine.identifier.name}"'
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
                                f'"{state_name}" of state machine "{state_machine.identifier.name}"'
                            ),
                            Severity.ERROR,
                            Integration._to_location(package),
                        ),
                    )
