#!/usr/bin/env python3

import re

VERBOSE = False
REGISTERS={}

for i in range(64): REGISTERS["R%d" % i] = i


REGISTERS["ZERO"] = REGISTERS["R0"]
REGISTERS["RV"] = REGISTERS["R1"]
REGISTERS["RA"] = REGISTERS["R2"]
REGISTERS["SP"] = REGISTERS["R3"]
REGISTERS["GP"] = REGISTERS["R4"]
REGISTERS["FP"] = REGISTERS["R5"]

REGISTERS["A0"] = REGISTERS["R16"]
REGISTERS["A1"] = REGISTERS["R17"]
REGISTERS["A2"] = REGISTERS["R18"]
REGISTERS["A3"] = REGISTERS["R19"]
REGISTERS["A4"] = REGISTERS["R20"]
REGISTERS["A5"] = REGISTERS["R21"]
REGISTERS["A6"] = REGISTERS["R22"]
REGISTERS["A7"] = REGISTERS["R23"]
REGISTERS["A8"] = REGISTERS["R24"]
REGISTERS["A9"] = REGISTERS["R25"]
REGISTERS["A10"]= REGISTERS["R26"]
REGISTERS["A11"]= REGISTERS["R27"]
REGISTERS["A12"]= REGISTERS["R28"]
REGISTERS["A13"]= REGISTERS["R29"]
REGISTERS["A14"]= REGISTERS["R30"]
REGISTERS["A15"]= REGISTERS["R31"]

REGISTERS["T0"] = REGISTERS["R32"]
REGISTERS["T1"] = REGISTERS["R33"]
REGISTERS["T2"] = REGISTERS["R34"]
REGISTERS["T3"] = REGISTERS["R35"]
REGISTERS["T4"] = REGISTERS["R36"]
REGISTERS["T5"] = REGISTERS["R37"]
REGISTERS["T6"] = REGISTERS["R38"]
REGISTERS["T7"] = REGISTERS["R39"]
REGISTERS["T8"] = REGISTERS["R40"]
REGISTERS["T9"] = REGISTERS["R41"]
REGISTERS["T10"]= REGISTERS["R42"]
REGISTERS["T11"]= REGISTERS["R43"]
REGISTERS["T12"]= REGISTERS["R44"]
REGISTERS["T13"]= REGISTERS["R45"]
REGISTERS["T14"]= REGISTERS["R46"]
REGISTERS["T15"]= REGISTERS["R47"]

REGISTERS["S0"] = REGISTERS["R48"]
REGISTERS["S1"] = REGISTERS["R49"]
REGISTERS["S2"] = REGISTERS["R50"]
REGISTERS["S3"] = REGISTERS["R51"]
REGISTERS["S4"] = REGISTERS["R52"]
REGISTERS["S5"] = REGISTERS["R53"]
REGISTERS["S6"] = REGISTERS["R54"]
REGISTERS["S7"] = REGISTERS["R55"]
REGISTERS["S8"] = REGISTERS["R56"]
REGISTERS["S9"] = REGISTERS["R57"]
REGISTERS["S10"]= REGISTERS["R58"]
REGISTERS["S11"]= REGISTERS["R59"]
REGISTERS["S12"]= REGISTERS["R60"]
REGISTERS["S13"]= REGISTERS["R61"]
REGISTERS["S14"]= REGISTERS["R62"]
REGISTERS["S15"]= REGISTERS["R63"]

class Statement:
    def __init__(self, statement):
        self._word_address = None
        self._statement = statement

    @property
    def word_address(self):
        return self._word_address
    @word_address.setter
    def word_address(self, value):
        if value != None and not isinstance(value, int):
            raise TypeError("Word address must be an int")
        self._word_address = value

    @property
    def statement(self):
        return self._statement

    def generate_output_code(self, labels={}):
        raise NotImplementedError()


class Label(Statement):
    def __init__(self, statement, label):
        super().__init__(statement)
        self._label = label

    @property
    def label(self):
        return self._label

    def __str__(self):
        return "Label(%s)" % self._label

    def __repr__(self):
        return __str__(self)


class Instruction(Statement):
    def __init__(self, statement, op, args=[]):
        super().__init__(statement)
        self._op = op
        self._args = args

    def generate_iword(self, labels):
        raise NotImplementedError()

    def generate_output_code(self, labels={}):
        byte_address = "0x" + int2hex(self.word_address*4, 8)
        out = "-- @ " + byte_address + " : " + self.statement.upper()
        out += "\n"
        out += hex(self.word_address)[2:].zfill(8) + ' : ' + self.generate_iword(labels) + ';'
        return out

    @property
    def op(self):
        return self._op

    @property
    def args(self):
        return self._args

    def __str__(self):
        return "0x%08x: %-20s (%5s: %s)" % (self.word_address, self.__class__.__name__, self.op, str(self.args))

    def __repr__(self):
        return str(self)

OPCODES = {
    'ADD': "100000",
    'SUB': "101000",
    'AND': "100100",
    'OR' : "100101",
    'XOR': "100110",
    'NAND':"101100",
    'NOR' :"101101",
    'NXOR' :"101110",

    'ADDI': "100000",
    'ANDI': "100100",
    'ORI' : "100101",
    'XORI' :"100110",
    
    'LW': "010010",
    'SW': "011010",

    'EQ' :"001000",
    'LT' :"001001",
    'LE' :"001010",
    'NE' :"001011",
    'BEQ': "001000",
    'BLT': "001001",
    'BLE': "001010",
    'BNE': "001011",
    'JAL': "001100"
}
OPCODES = {instr: hex(int(val.replace(' ', ''), 2))[2:].zfill(2) for instr, val in OPCODES.items()}

PC_RELATIVE_INSTRUCTIONS = [
    'BF',
    'BEQ',
    'BLT',
    'BLE',
    'BT',
    'BNE',
    'BGE',
    'BGT',
    #'BEQZ',
    #'BLTZ',
    #'BLEZ',
    #'BNEZ',
    #'BGEZ',
    #'BGTZ'
]


def reg2hex(regname):
    return int2hex(REGISTERS[regname], 1)


def int2hex(val, numchars):
    return hex(val)[2:].zfill(numchars)


reg3ops = ['ADD', 'SUB', 'AND', 'OR', 'XOR', 'NAND', 'NOR', 'XNOR', 'EQ', 'LT', 'LE', 'NE', 'GTE', 'GT']
reg2immops = ['ADDI', 'SUBI', 'ANDI', 'ORI', 'XORI', 'NANDI', 'NORI', 'XNORI', 'LW', 'SW', 'FI', 'EQI', 'LTI', 'LTEI',
              'TI', 'NEI', 'GTEI', 'GTI', 'BF', 'BEQ', 'BLT', 'BLTE', 'BT', 'BNE', 'BGTE', 'BGT', 'JAL']
regimmops = ['BEQZ', 'BLTZ', 'BLEZ', 'BNEZ', 'BGEZ', 'BGTZ']

class InstructionReg3(Instruction):
    def generate_iword(self, labels):
        if self.op not in reg3ops:
            raise Exception("Invalid instruction usage '%s' at instruction '%s'" % (self.op, self.statement))

        return ''.join([reg2hex(self.args[i]) for i in range(3)]) + '000' + OPCODES[self.op]

class InstructionReg2Imm(Instruction):
    def generate_iword(self, labels):
        if self.op not in reg2immops:
            raise Exception("Invalid instruction usage '%s' at instruction '%s'" % (self.op, self.statement))
        
        args = [self.args[1], self.args[0], self.args[2]] if self.op == 'SW' else self.args

        imm = self.args[2]
        if imm[0:2] == '0x':
            imm = imm[2:].zfill(4)
        else:
            if imm not in labels:
                raise Exception("Nonexistent label used '%s' at instruction '%s'" % (imm, self.statement))

            val = labels[imm]
            if self.op in PC_RELATIVE_INSTRUCTIONS: val -= self.word_address + 1
            imm = int2hex(val & 0xffffffff, 4) # ?
		
        return ''.join([reg2hex(args[i]) for i in range(2)]) + imm[-4:] + OPCODES[self.op]

class InstructionRegImm(Instruction):
    def generate_iword(self, labels):
        if self.op not in regimmops:
            raise Exception("Invalid instruction usage '%s' at instruction '%s'" % (self.op, self.statement))
        
        imm = self.args[1]
        if imm[0:2] == '0x':
            imm = imm[2:].zfill(8)
        else:
            if imm not in labels:
                raise Exception("Nonexistent label used '%s' at instruction '%s'" % (imm, self.statement))
            
            val = labels[imm]
            if self.op in PC_RELATIVE_INSTRUCTIONS: val -= self.word_address + 1
            imm = int2hex(val & 0xffffffff, 8)
		
        return reg2hex(self.args[0]) + '0' + (imm[0:4] if self.op == 'MVHI' else imm[-4:]) + OPCODES[self.op]

class InstructionImm(Instruction):
    pass

class InstructionReg2(Instruction):
    pass

class Directive(Instruction):
    def generate_iword(self, labels):
        if self.op == '.WORD':
            return self.args[0][2:].zfill(8)[-8:]
        else:
            raise NotImplementedError()


def generate_output(statements, labels={}):
    result = """WIDTH=32;
DEPTH=2048;
ADDRESS_RADIX=HEX;
DATA_RADIX=HEX;
CONTENT BEGIN\n"""


    statements.sort(key=lambda s: s.word_address)

    def emit_deadspace(start, end):
        nonlocal result
        if start == end:
            result += "%08x : DEAD;\n" % start
        elif start < end:
            result += "[%08x..%08x] : DEAD;\n" % (start, end)


    prev_addr = None
    for stmt in statements:
        if prev_addr == None:
            if stmt.word_address != 0:
                emit_deadspace(0, stmt.word_address - 1)
        elif prev_addr < stmt.word_address -1:
            emit_deadspace(prev_addr + 1, stmt.word_address - 1)

        result += stmt.generate_output_code(labels) + "\n"
        prev_addr = stmt.word_address

    emit_deadspace(prev_addr + 1, 2047)

    result += "END;\n"

    return result


def create_label_parser():
    label = r'^(\w+):$'
    return re.compile(label)

opcode = r'([A-Z]+)'
reg = '(' + '|'.join(list(zip(*REGISTERS.items()))[0]) + ')'
imm = r'(0x[0-9A-F]+)|(-?\d+)|(\w+)'

def create_instr_regs_parser():
    instr = r'^' + opcode + r'\s+' + reg + r'(?:,\s*' + reg + r')?,\s*' + r'(?:' + reg + r'|' + imm + r')$'
    return re.compile(instr, re.I)

def create_instr_addr_parser():
    instr = r'^' + opcode + r'\s+(?:' + reg + r',\s*)?(?:' + imm + r')\s*\(\s*' + reg + r'\s*\)$'
    return re.compile(instr, re.I)

def create_instr_br_parser():
    instr = r'^BR\s*(?:' + imm + ')$'
    return re.compile(instr, re.I)

def create_directive_parser():
    directive = r'^\s*\.(?:ORIG\s+(?:(0x[0-9a-fA-F]+)|(-?\d+))|WORD\s+(?:' + imm + r')|NAME\s+(\w+)\s*=\s*(?:(0x[0-9a-fA-F]+)|(-?\d+)))\s*$'
    return re.compile(directive, re.I)

def clean(lines):
    lines = [l.split(';')[0] for l in lines]
    i = 0
    while i < len(lines):
        idx = lines[i].find(':')
        if idx != -1:
            lines.insert(i+1, lines[i][idx+1:])
            lines[i] = lines[i][0:idx+1]
        i += 1

    return [l.strip() for l in lines if l.strip()]

def parse_statements(lines):
    instr_regs_parser = create_instr_regs_parser()
    instr_addr_parser = create_instr_addr_parser()
    instr_br_parser = create_instr_br_parser()
    label_parser = create_label_parser()
    directive_parser = create_directive_parser()

    statements = []
    for l, i in zip(lines, range(1, len(lines) + 1)):
        l = ' '.join([p for p in l.split() if len(p) > 0])

        match = instr_regs_parser.match(l)
        if match:
            value = hex(int(match.group(5) or match.group(6), 0) & 0xffff) if match.group(5) or match.group(6) else match.group(7)

            instrClass = None
            args = None

            if value:
                if match.group(3):
                    instrClass = InstructionReg2Imm
                    args = [match.group(2).upper(), match.group(3).upper(), value]
                else:
                    instrClass = InstructionRegImm
                    args = [match.group(2).upper(), value]
            else:
                if match.group(3):
                    instrClass = InstructionReg3
                    args = [match.group(2).upper(), match.group(3).upper(), match.group(4).upper()]
                else:
                    instrClass = InstructionReg2
                    args = [match.group(2).upper(), match.group(4).upper()]

            statements.append(instrClass(l, match.group(1).upper(), args))
            continue

        match = instr_addr_parser.match(l)
        if match:
            value = hex(int(match.group(3) or match.group(4), 0) & 0xffff) if match.group(3) or match.group(4) else match.group(5)

            instrClass = None
            args = None

            if match.group(2):
                instrClass = InstructionReg2Imm
                args = [match.group(2).upper(), match.group(6).upper(), value]
            else:
                instrClass = InstructionRegImm
                args = [match.group(6).upper(), value]

            statements.append(instrClass(l, match.group(1).upper(), args))
            continue

        match = instr_br_parser.match(l)
        if match:
            value = hex(int(match.group(1) or match.group(2), 0) & 0xffff) if match.group(1) or match.group(2) else match.group(3)
            statements.append(InstructionImm(l, 'BR', [value]))
            continue

        match = label_parser.match(l)
        if match:
            label = match.group(1)
            if label in REGISTERS or label in OPCODES:
                raise Exception("Cannot use reserved keyword as label at statement %d: %s" % (i, l))

            statements.append(Label(l, label))
            continue

        match = directive_parser.match(l)
        if match:
            if match.group(1) != None or match.group(2) != None:
                value = hex(int(match.group(1) or match.group(2), 0) & 0xffffffff)
                statements.append(Directive(l, '.ORIG', [value]))
            elif match.group(3) != None or match.group(4) != None or match.group(5) != None:
                value = hex(int(match.group(3) or match.group(4), 0) & 0xffffffff) if match.group(3) or match.group(4) else match.group(5)
                statements.append(Directive(l, '.WORD', [value]))
            else:
                value = hex(int(match.group(7) or match.group(8), 0) & 0xffffffff)

                name = match.group(6)
                if name in REGISTERS: # or name in OPCODES (removed b/c 'SW')
                    raise Exception("Cannot use reserved keyword as name at statement %d: %s" % (i, l))

                statements.append(Directive(l, '.NAME', [name, value]))

            continue

        match = re.match(r'^(RET)$', l, re.I)
        if match:
            statements.append(Instruction(l, 'RET'))
            continue


    return statements

def expand_pseudo_ops(statements):
    newStatements = []
    for s in statements:
        if isinstance(s, Instruction):
            if s.op == 'BR':
                newStatements.append(InstructionReg2Imm(s.statement, 'BEQ', ['R6', 'R6', s.args[0]]))
            elif s.op == 'SUBI':
                newStatements.append(InstructionReg2Imm(s.statement, 'ADDI', [s.args[1], s.args[0], s.args[2]]))
            elif s.op == 'NXOR':
                newStatements.append(InstructionReg3(s.statement, 'LT', [s.args[0], s.args[1], s.args[2]]))
            elif s.op == 'NOT':
                newStatements.append(InstructionReg3(s.statement, 'NAND', [s.args[0], s.args[1], s.args[1]]))
            elif s.op == 'GT':
                newStatements.append(InstructionReg3(s.statement, 'LT', [s.args[0], s.args[1], s.args[2]]))
            elif s.op == 'GE':
                newStatements.append(InstructionReg3(s.statement, 'LE', [s.args[0], s.args[1], s.args[2]]))
            elif s.op == 'BGT':
                newStatements.append(InstructionReg3(s.statement, 'BLT', [s.args[1], s.args[0], s.args[2]]))
            elif s.op == 'BGE':
                newStatements.append(InstructionReg3(s.statement, 'BLE', [s.args[1], s.args[0], s.args[2]]))
            elif s.op == 'CALL':
                newStatements.append(InstructionReg2Imm(s.statement, 'JAL', ['RA', s.args[0], s.args[1]]))
            elif s.op == 'RET':
                newStatements.append(InstructionReg2Imm(s.statement, 'JAL', ['R9', 'RA', '0x0']))
            elif s.op == 'JMP':
                newStatements.append(InstructionReg2Imm(s.statement, 'JAL', ['R9', s.args[0], s.args[1]]))
            elif not isinstance(s, Directive) and s.op not in OPCODES:
                    raise Exception("Invalid opcode %s at statement %s" % (s.op, s.statement))
            else:
                newStatements.append(s)
        else:
            newStatements.append(s)

    return newStatements

def assign_addresses(statements):
    physical_statements = []
    labels = {}

    current_address = None

    for s in statements:
        if isinstance(s, Directive):
            if s.op == '.ORIG':
                current_address = int(int(s.args[0], 0) / 4)
            elif s.op == '.NAME':
                labels[s.args[0]] = int(s.args[1], 0)
            elif s.op == '.WORD':
                if current_address == None:
                    raise Exception(".WORD directive found before .ORIG %s" % str(s))

                s.word_address = current_address
                physical_statements.append(s)
                current_address += 1
        elif isinstance(s, Instruction):
            if current_address == None:
                raise Exception("Instruction found before .ORIG %s" % str(s))

            s.word_address = current_address
            physical_statements.append(s)
            current_address += 1
        elif isinstance(s, Label):
            if current_address == None:
                raise Exception("Label found before .ORIG %s" % str(s))

            labels[s.label] = current_address

    return (physical_statements, labels)

def assemble(fileIn, fileOut):
    lines = clean([l for l in open(fileIn)])
    if VERBOSE: print(repr(lines))

    statements = parse_statements(lines)
    statements = expand_pseudo_ops(statements)

    statements, labels = assign_addresses(statements)

    if VERBOSE:
        print("\nStatements:")
        for s in statements:
            print(str(s))

        print("\nLabels:")
        for l, v in labels.items():
            print("0x%08x: %s" % (v, l))

    output = generate_output(statements, labels)

    with open(fileOut, 'w') as f:
        f.write(output)


if __name__ == '__main__':
    import sys

    if len(sys.argv) != 3:
        print("Usage: assembler.py inputFile outputFile")
        sys.exit(-1)

    try:
        assemble(sys.argv[1], sys.argv[2])
    except Exception as e:
        print("Error occurred while assembling: %s" % str(e))
        import traceback
        traceback.print_exc(file=sys.stdout)
        sys.exit(-1)