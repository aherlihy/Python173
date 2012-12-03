try:
    print("trying")
    raise Exception("BAD!")
except:
    print("except")
else:
    print("else")
