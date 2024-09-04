#!/bin/bash

# ----------------------------------------------------------------------------
#  Purpose:      Download the Python dependencies and archive the results.
#
#  Dependencies: Python 3.9 - This is the version that is currently
#                expected by the Anod spec.
#
#  Arguments:
#      1. TARGET_DIR     - Directory to create archive into
#      2. ARCHIVE_NAME   - Name of the archive to create
# ----------------------------------------------------------------------------

set -euo pipefail

# Check for required arguments
if [[ $# -ne 2 ]]; then
    echo "Usage: $0 <TARGET_DIR> <ARCHIVE_NAME>" >&2
    exit 1
fi

TARGET_DIR=$1
ARCHIVE_NAME=$2

PYTHON_VERSION="3.9"
PYTHON_EXECUTABLE="${PYTHON_EXECUTABLE:-python${PYTHON_VERSION}}"

# Validate Python availability
if ! command -v ${PYTHON_EXECUTABLE} >/dev/null 2>&1; then
    echo "Error: ${PYTHON_EXECUTABLE} is not found. Please install it and try again." >&2
    exit 1
fi

# Validate Python version
if [ -z "$(${PYTHON_EXECUTABLE} --version | grep "^Python ${PYTHON_VERSION}")" ]; then
    echo "Error: Python ${PYTHON_VERSION} is required, but $(${PYTHON_EXECUTABLE} --version) is used." >&2;
    exit 1;
fi

mkdir -p "${TARGET_DIR}"

TARGET_DIR=$(realpath ${TARGET_DIR})
REQUIREMENTS_RFLX="${TARGET_DIR}/requirements-rflx.txt"
REQUIREMENTS_EXTRA="${TARGET_DIR}/requirements-extra.txt"

# Prepare for download

# The RecordFlux requirements must be split into binary and source lists first.
# The packages that are excluded below do not have a binary version (wheel)
# available for the platform that we need.

make anod_rflx_dependencies | grep -vE "(Leaving|Entering) directory" > ${REQUIREMENTS_RFLX}
make anod_extra_dependencies | grep -vE "(Leaving|Entering) directory" > ${REQUIREMENTS_EXTRA}

REQUIREMENTS_RFLX_BIN="${TARGET_DIR}/requirements-rflx-bin.txt"
REQUIREMENTS_RFLX_SRC="${TARGET_DIR}/requirements-rflx-src.txt"

cat ${REQUIREMENTS_RFLX} \
    | grep -v "ld==" | grep -v "pydotplus==" | grep -v "e3-core" \
    > ${REQUIREMENTS_RFLX_BIN}
grep -vFf ${REQUIREMENTS_RFLX_BIN} ${REQUIREMENTS_RFLX} > ${REQUIREMENTS_RFLX_SRC}

cd "${TARGET_DIR}"

WHEELS_DIR="${TARGET_DIR}/wheels"
mkdir -p "${WHEELS_DIR}"

# Download
# Note: The platforms here are chosen to support older systems with libc 2.17 (e.g. RHEL 7)
${PYTHON_EXECUTABLE} -m pip download --platform manylinux_2_17_x86_64 --platform manylinux1_x86_64 --platform manylinux2014_x86_64 --disable-pip-version-check --python-version=${PYTHON_VERSION} --only-binary=:all: -r "${REQUIREMENTS_EXTRA}" -d ${WHEELS_DIR}
${PYTHON_EXECUTABLE} -m pip download --platform manylinux_2_17_x86_64 --platform manylinux1_x86_64 --platform manylinux2014_x86_64 --disable-pip-version-check --python-version=${PYTHON_VERSION} --only-binary=:all: -r "${REQUIREMENTS_RFLX_BIN}" -d ${WHEELS_DIR}
${PYTHON_EXECUTABLE} -m pip download --disable-pip-version-check -r "${REQUIREMENTS_RFLX_SRC}" -d ${WHEELS_DIR}

# Create the archive
tar -czf "${TARGET_DIR}/${ARCHIVE_NAME}" -C "${TARGET_DIR}" wheels

echo "Created archive ${ARCHIVE_NAME}"
