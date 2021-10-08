import os


ALL_LABELS = [
    'start',
    'stop',
    'summary',
    'description',
    'location',
    'date',
    'allDay',
    'time',
    'timeZone',
    'repeat',
    'frequency',
    'interval',
]


def emit_def(label):
    return ''.join([
        label,
        " = Accessors.makeOneToOne .",
        label,
        " (\\c r -> { r | ", label, " = c r.", label, " })"])


try:
    os.mkdir('gen')
except FileExistsError:
    pass


with open("gen/GenAccessors.elm", "w") as f:
    f.write("module GenAccessors exposing(..)\n\n")
    f.write("import Accessors\n\n")
    for label in ALL_LABELS:
        f.write(emit_def(label))
        f.write("\n\n")
