def make_adder5(x):
    class Adder:
        def __call__(self, y):
            return x + y
    return Adder()

a = make_adder5(1)
plus10 = make_adder5(10)
___assertEqual(a(1), 2) #passing self into --call--?
___assertEqual(plus10(-2), 8)
