
def api(obj, max=40):
    contents = sorted(dir(obj))
    dunders = [x for x in contents if x.startswith("__")]
    private = [x for x in contents if x.startswith("_") and not x.startswith("__")]
    public  = [x for x in contents if not x.startswith("_")]
    print("Dunders: ", end="")
    total = 0
    for x in dunders:
        if total > max:
            print("\n         ", end="")
            total = 0

        print(x, end=" ")
        total += len(x)

    print()
    print("Private: ", end="")
    total = 0
    for x in private:
        if total > max:
            print("\n         ", end=" ")
            total = 0
        print(x, end="")
        total += len(x)

    print()
    print("Public: ", end=" ")
    total = 0
    for x in public:
        if total > max:
            print("\n        ", end=" ")
            total = 0
        print(x, end=" ")
        total += len(x)

    print()
