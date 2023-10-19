import sys
from parsefile import *

if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.stderr.write('usage: %s filename parsername' % sys.argv[0])
        sys.exit(1)
    filename = sys.argv[1]
    with open(filename) as file:
        for line in file:
            characters = line.strip()  # Read and strip each line
            if not characters:
                continue  # Skip empty lines
            tokens = imp_lex(characters)
            print(tokens)
            parser = globals()[sys.argv[2]]()
            result = parser(tokens, 0)
            print(result)