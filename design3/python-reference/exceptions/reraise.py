try:
    try:
        raise IndexError()
    except IndexError as e:
        exc1 = e
        raise
except IndexError as exc2:
    ___assertTrue(exc1 is exc2)
else:
    ___fail("No exception raised")
#tests variables stored within a try statement should be stored in the scope above
