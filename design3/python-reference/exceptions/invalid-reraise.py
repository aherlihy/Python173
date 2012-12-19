try:
    raise
except RuntimeError as e:
    ___assertIn("No active exception", str(e))
else:
    ___fail("No exception raised")
#tests str, ___fail, and saving the exception as e
