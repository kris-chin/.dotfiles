import os

#Helper class that takes a dictionary and converts its keys into properties
class DictToObject:
    def __init__(self, **kwargs):
        for key, value in kwargs.items():
            setattr(self, key, value)


#reads an ;-seperated environment variable string and interprets it as an array
def readArrayEnvVar(varName):
    arrayVar = os.environ.get(varName)
    if arrayVar:
        return arrayVar.split(";")
    return None
