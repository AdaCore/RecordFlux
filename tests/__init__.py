import logging

import pytest

logging.disable(logging.CRITICAL)

pytest.register_assert_rewrite("tests.utils")
