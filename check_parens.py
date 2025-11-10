#!/usr/bin/env python3
import sys

def check_parens(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
        depth = 0
        in_string = False
        in_comment = False
        escape_next = False
        
        for i, line in enumerate(lines, 1):
            for j, char in enumerate(line):
                if escape_next:
                    escape_next = False
                    continue
                    
                if char == '\\':
                    escape_next = True
                    continue
                
                # Handle comments
                if char == ';' and not in_string:
                    in_comment = True
                    
                if char == '\n':
                    in_comment = False
                    continue
                    
                if in_comment:
                    continue
                
                # Handle strings
                if char == '"' and not in_string:
                    in_string = True
                    continue
                elif char == '"' and in_string:
                    in_string = False
                    continue
                    
                if in_string:
                    continue
                
                # Count parens
                if char == '(':
                    depth += 1
                elif char == ')':
                    depth -= 1
                    
                if depth < 0:
                    print(f'Line {i}, col {j}: depth went negative!')
                    print(f'Line content: {line.rstrip()}')
                    return False
                    
        print(f'Final depth: {depth}')
        if depth != 0:
            print(f'ERROR: Unmatched parentheses! Final depth: {depth}')
            return False
        else:
            print('OK: All parentheses matched')
            return True

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Usage: check_parens.py <file>')
        sys.exit(1)
    
    if check_parens(sys.argv[1]):
        sys.exit(0)
    else:
        sys.exit(1)

