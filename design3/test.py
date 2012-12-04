try:
    print("try")
    raise KeyError("err")
except KeyError:
    print("except")
finally:
    print("final")
