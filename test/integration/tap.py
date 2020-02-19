"""
Module for producing output according to the Test Anything Protocol.
The program prove(1) can process this output.
"""


_test_number = 0


def plan(n: int) -> None:
    """Print the number of tests that are to be run."""
    print(f"1..{n}")


def ok(message: str) -> None:
    """Report that the current test succeeded."""
    global _test_number
    _test_number += 1
    print(f"ok {_test_number} - {message}")


def not_ok(message: str) -> None:
    """Report that the current test failed."""
    global _test_number
    _test_number += 1
    print(f"not ok {_test_number} - {message}")


def diagnose(message: str) -> None:
    """Print debug output."""
    print(f"# {message}")
