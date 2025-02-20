lines=""

with open("hello.cl-ast") as f:
    lines=f.readlines()

# type checking - ensure that data types are used consistently.

"""
consider errors related to classes and attributes
    without looking at expressions


"""

print(lines)