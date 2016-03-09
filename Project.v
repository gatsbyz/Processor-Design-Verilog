module Project(
	input        CLOCK_50,
	input        RESET_N,
	input  [3:0] KEY,
	input  [9:0] SW,
	output [6:0] HEX0,
	output [6:0] HEX1,
	output [6:0] HEX2,
	output [6:0] HEX3,
	output [6:0] HEX4,
	output [6:0] HEX5,
	output [9:0] LEDR
);

  parameter DBITS    =32;
  parameter INSTSIZE =32'd4;
  parameter INSTBITS =32;
  parameter REGNOBITS=6;
  parameter IMMBITS  =14;
  parameter STARTPC  =32'h100;
  parameter ADDRHEX  =32'hFFFFF000;
  parameter ADDRLEDR =32'hFFFFF020;
  parameter ADDRKEY  =32'hFFFFF080;
  parameter ADDRSW   =32'hFFFFF090;
  // Change this to Sorter2.mif before submitting
  parameter IMEMINITFILE="Test2.mif";
  parameter IMEMADDRBITS=16;
  parameter IMEMWORDBITS=2;
  parameter IMEMWORDS=(1<<(IMEMADDRBITS-IMEMWORDBITS));
  parameter DMEMADDRBITS=16;
  parameter DMEMWORDBITS=2;
  parameter DMEMWORDS=(1<<(DMEMADDRBITS-DMEMWORDBITS));
  
  parameter OP1BITS=6;
  parameter OP1_ALUR =6'b000000;
  parameter OP1_ALUI =6'b000000;
  parameter OP1_BEQ  =6'b001000;
  parameter OP1_BLT  =6'b001001;
  parameter OP1_BLE  =6'b001010;
  parameter OP1_BNE  =6'b001011;
  parameter OP1_JAL  =6'b001100;
  parameter OP1_LW   =6'b010010;
  parameter OP1_SW   =OP1_LW+6'b001000;
  parameter OP1_ADDI =6'b100000;
  parameter OP1_ANDI =6'b100100;
  parameter OP1_ORI  =6'b100101;
  parameter OP1_XORI =6'b100110;
  
  // Add parameters for secondary opcode values
  parameter OP2_ADD	=6'b100000;
  parameter OP2_SUB	=6'b101000;
  parameter OP2_AND	=6'b100100;
  parameter OP2_OR	=6'b100101;
  parameter OP2_XOR	=6'b100110;
  parameter OP2_NAND=6'b101100;
  parameter OP2_NOR	=6'b101101;
  parameter OP2_NXOR=6'b101110;
  
  // CMP/CMPi/Bcond Opcodes
  parameter OP2_EQ	=6'b001000;
  parameter OP2_LT	=6'b001001;
  parameter OP2_LE	=6'b001010;
  parameter OP2_NE	=6'b001011;
  
  // The reset signal comes from the reset button on the DE0-CV board
  // RESET_N is active-low, so we flip its value ("reset" is active-high)
  wire clk,locked;
  // The PLL is wired to produce clk and locked signals for our logic
  Pll myPll(
    .refclk(CLOCK_50),
	 .rst      (!RESET_N),
	 .outclk_0 (clk),
    .locked   (locked)
  );
  wire reset=!locked;
 
  // Create the processor's bus
  tri [(DBITS-1):0] thebus;
  parameter BUSZ={DBITS{1'bZ}};  
  // Create PC and connect it to the bus
  reg [(DBITS-1):0] PC;
  reg LdPC, DrPC, IncPC;
  always @(posedge clk or posedge reset) begin
    if(reset)
	   PC<=STARTPC;
	 else if(LdPC)
      PC<=thebus;
    else if(IncPC)
      PC<=PC+INSTSIZE;
  end
  assign thebus=DrPC?PC:BUSZ;
  // Create the instruction memory
  (* ram_init_file = IMEMINITFILE *)
  reg [(DBITS-1):0] imem[(IMEMWORDS-1):0];
  wire [(DBITS-1):0] iMemOut=imem[PC[(IMEMADDRBITS-1):IMEMWORDBITS]];

  // Create the IR (feeds directly from memory, not from bus)
  reg [(INSTBITS-1):0] IR;
  reg LdIR;
  always @(posedge clk or posedge reset)
    if(reset)
	   IR<=32'hDEADDEAD;
	 else if(LdIR)
      IR <= iMemOut;

  // Put the code for getting op1, rd, rs, rt, imm, etc. here
  wire [5:0] op1   =IR[31:26];
  wire [5:0] rd    =IR[25:20];
  wire [5:0] rs    =IR[19:14];
  wire [5:0] rt    =IR[13:8];
  wire [13:0]imm   =IR[13:0];
  wire [5:0] op2 =IR[5:0];
  wire [5:0] op2_d =rd;
  wire [5:0] op2_t =rt; 
  
  // Put the code for data memory and I/O here
  reg [(DBITS-1):0]  MAR;
  reg [(DBITS-1):0] dmem[(DMEMWORDS-1):0];
  reg WrMem;
  //reg [(DBITS-1):0] memin;
  wire MemEnable  =!(MAR[(DBITS-1):DMEMADDRBITS]); // If MAR upper bits are not all 0
  wire LEDREnable =(MAR==ADDRLEDR)&WrMem;
  wire HEXEnable  =(MAR==ADDRHEX)&WrMem;
  wire KEYState   =(MAR==ADDRKEY)&DrMem;
  wire SWITCHState=(MAR==ADDRSW)&DrMem;

  reg [9:0] GATS;
  reg [7:0] GREEN;
  reg [3:0] DIG0,DIG1,DIG2,DIG3,DIG4,DIG5;
  reg [1:0] test;
  assign LEDR[9:0]=GATS[9:0];
  //assign LEDG[7:0]=GREEN[7:0];

  //SevenSeg
  SevenSeg s0(.IN(DIG0), .OUT(HEX0));
  SevenSeg s1(.IN(DIG1), .OUT(HEX1));
  SevenSeg s2(.IN(DIG2), .OUT(HEX2));
  SevenSeg s3(.IN(DIG3), .OUT(HEX3));
  SevenSeg s4(.IN(DIG4), .OUT(HEX4));
  SevenSeg s5(.IN(DIG5), .OUT(HEX5));

  wire MemWE=WrMem&MemEnable&!reset;

  //Save block
  always @(posedge clk)begin
    if(LdMAR)
  	MAR<=thebus;
    if(LEDREnable)
	   GATS<=thebus[9:0];
    else if(HEXEnable)
	  {DIG5,DIG4,DIG3,DIG2,DIG1,DIG0}={thebus[23:0]};	
    else if(MemWE)
	   dmem[MAR[(DMEMADDRBITS-1):DMEMWORDBITS]]<=thebus;
  end
  wire [(DBITS-1):0] MemVal=
  KEYState?{28'b0,~KEY}:
  SWITCHState?{22'b0,SW}:
  MemWE?
  {DBITS{1'bX}}:
  dmem[MAR[(DMEMADDRBITS-1):DMEMWORDBITS]];

  assign thebus=DrMem?MemVal:BUSZ;
		
  // Create the registers and connect them to the bus
  reg [(DBITS-1):0] regs[63:0];
  reg [(REGNOBITS-1):0] regno;
  reg WrReg,DrReg;
  always @(posedge clk)
    if(WrReg&&!reset)
      regs[regno]<=thebus;
  wire [(DBITS-1):0] regout=WrReg?{DBITS{1'bX}}:regs[regno];
  assign thebus=DrReg?regout:BUSZ;
  
  // Create ALU unit and connect to the bus
  reg signed [(DBITS-1):0] A,B;
  reg LdA,LdB,DrALU;
  // Connect A and B registers to the bus
  always @(posedge clk or posedge reset)
    if(reset)
	   {A,B}<=32'hDEADDEAD;
	 else begin
      if(LdA)
        A <= thebus;
      if(LdB)
        B <= thebus;
    end
  // Connect ALU output to the bus (controlled by DrALU)
  reg signed [(DBITS-1):0] ALUout;
  assign thebus=DrALU?ALUout:BUSZ;

  // Put the code for the actual ALU here
  reg [(REGNOBITS-1):0] ALUfunc;
  //reg [(DBITS-1):0] ALUout;
  reg ALUstate;
  always @(ALUfunc or ALUstate or A or B or DrALU) begin
//		GP[3:0]=ALUfunc[3:0];
//		GP[4]=ALUstate;
//		GP[5]=DrALU;
    if(ALUstate==1'b1) begin  //too much hardware?
	   case(ALUfunc)
		  //OP2_F:		ALUout<=32'h00000000;
		  //OP2_T:		ALUout<=32'h00000001;
		  OP2_EQ:	ALUout<=(A==B);
		  OP2_LT:	ALUout<=(A<B);
		  //OP2_LTE:	ALUout<=(A<=B);
		  //OP2_EQZ:	ALUout<=(A==0);
		  //OP2_LTZ:	ALUout<=(A<0);
		  //OP2_LTEZ:	ALUout<=(A<=0);
		  //OP2_NE:	ALUout<=(A!=B);
		  //OP2_GTE:	ALUout<=(A>=B);
		  //OP2_GT:	ALUout<=(A>B);
		  //OP2_NEZ:	ALUout<=(A!=0);
		  //OP2_GTEZ:	ALUout<=(A>=0);
		  //OP2_GTZ:	ALUout<=(A>0);
		  default:  ALUout<=(A+B);
		endcase	
    end else begin
	   case(ALUfunc)
		  OP2_ADD:	ALUout<=(A+B);
		  OP2_SUB:	ALUout<=(A-B);
		  OP2_AND:	ALUout<=(A&B);
		  OP2_OR:	ALUout<=(A|B);
		  OP2_XOR:	ALUout<=(A^B);
		  OP2_NAND: ALUout<=~(A&B);
		  OP2_NOR:	ALUout<=~(A|B);
		  OP2_NXOR: ALUout<=~(A^B);
		  default:  ALUout<=(A+B);
		endcase
	 end
  end
	
  assign thebus=DrALU?ALUout:BUSZ;
  
  parameter S_BITS=5;
  parameter [(S_BITS-1):0]
    S_ZERO  ={(S_BITS){1'b0}},
    S_ONE   ={{(S_BITS-1){1'b0}},1'b1},
    S_FETCH1=S_ZERO,				// 00
	 S_FETCH2=S_FETCH1+S_ONE,  // 01
    S_ALUR1 =S_FETCH2+S_ONE,	// 02
    S_ALUR2 =S_ALUR1 +S_ONE,	// 03
    S_ALUI1 =S_ALUR2 +S_ONE,	// 04
    S_ALUI2 =S_ALUI1 +S_ONE,	// 05
    S_CMPR1 =S_ALUI2 +S_ONE,	// 06
    S_CMPR2 =S_CMPR1 +S_ONE,	// 07
    S_CMPI1 =S_CMPR2 +S_ONE,	// 08
    S_CMPI2 =S_CMPI1 +S_ONE,	// 09
	 S_BCOND1=S_CMPI2 +S_ONE,  // 0A
	 S_BCOND2=S_BCOND1+S_ONE,  // 0B
	 S_BCOND3=S_BCOND2+S_ONE,  // 0C
	 S_BCOND4=S_BCOND3+S_ONE,  // 0D
    S_LW1   =S_BCOND4+S_ONE,	//	0E
    S_LW2   =S_LW1   +S_ONE,	//	0F
    S_LW3   =S_LW2   +S_ONE,	//	10
    S_SW1   =S_LW3   +S_ONE,	//	11
    S_SW2   =S_SW1   +S_ONE,	//	12
    S_SW3   =S_SW2   +S_ONE,	//	13
    S_JAL1  =S_SW3   +S_ONE,	//	14
    S_JAL2  =S_JAL1  +S_ONE,	//	15
    S_JAL3  =S_JAL2  +S_ONE,	//	16
    S_ERROR =S_JAL3  +S_ONE;	//	17

	 // Put parameters for the remaining state names above
	 
  reg [(S_BITS-1):0] state,next_state;
  reg LdMAR,DrOff,ShOff,DrMem;
  assign thebus=DrOff?{{14{imm[13]}},imm[13:0]}:BUSZ;
  assign thebus=ShOff?{{12{imm[13]}},imm[13:0],2'b0}:BUSZ;
  always @(state or op1 or rs or rt or rd or op2 or op2_d or op2_t or ALUout[0]) begin
    {ALUstate,LdPC,DrPC,IncPC,LdMAR,WrMem,DrMem,LdIR,DrOff,ShOff, LdA, LdB,ALUfunc,DrALU,regno,DrReg,WrReg,next_state}=
    {1'b0    ,1'b0,1'b0, 1'b0, 1'b0, 1'b0, 1'b0,1'b0, 1'b0, 1'b0,1'b0,1'b0,   6'bX,1'b0,  6'bX, 1'b0, 1'b0,state+S_ONE};
    case(state)
  	   S_FETCH1: {LdIR,IncPC}={1'b1,1'b1};
	   S_FETCH2: begin
		  case(op1)
		    OP1_ALUR: begin
		      case(op2)
		        OP2_SUB,OP2_NAND,OP2_NOR,OP2_NXOR,
		        OP2_EQ,OP2_LT,OP2_LE,OP2_NE,
		        OP2_ADD,OP2_AND,OP2_OR,OP2_XOR:
			       next_state=S_ALUR1;
		        default: next_state=S_ERROR;
		      endcase
	       end
	       OP1_ADDI,OP1_ANDI,OP1_ORI,OP1_XORI:
		        next_state=S_ALUI1;
			 OP1_BEQ,OP1_BLT,OP1_BLE,OP1_BNE:
			   next_state=S_ALUI1;
			 OP1_JAL:
			   next_state=S_ALUI1;	     
			 OP1_LW:
			   next_state=S_ALUI1;
			 OP1_SW:
			   next_state=S_ALUI1;
		  endcase
			 
						
	// Put the code for the rest of the "dispatch" here
	  {regno,DrReg,LdA,LdB}={rs,1'b1,1'b1,1'b1};
      end
	// Put the rest of the "microcode" here
    S_ALUR1:  {regno,DrReg,LdB,next_state}={rt,1'b1,1'b1,S_ALUR2};
	 S_ALUR2:  {ALUfunc,DrALU,regno,WrReg,next_state}={op2,1'b1,rd,1'b1,S_FETCH1};
	 S_ALUI1: {DrOff,LdB,next_state}={1'b1,1'b1,S_ALUI2};
	 S_ALUI2: {DrALU,ALUfunc,regno,WrReg,next_state}={1'b1,op2_t,rd,1'b1,S_FETCH1};
	 
	 S_SW1:	{DrOff,LdB,next_state}={1'b1,1'b1,S_SW2};
	 S_SW2:	{DrALU,ALUfunc,LdMAR,next_state}={1'b1,OP2_ADD,1'b1,S_SW3};
	 S_SW3:	{WrMem,DrReg,regno,next_state}={1'b1,1'b1,rt,S_FETCH1};
	 
	 S_LW1:	{DrOff,LdB,next_state}={1'b1,1'b1,S_LW2};
	 S_LW2:	{DrALU,ALUfunc,LdMAR,next_state}={1'b1,OP2_ADD,1'b1,S_LW3};
	 S_LW3:	{WrReg,regno,DrMem,next_state}={1'b1,rd,1'b1,S_FETCH1};
	 
	 //S_CMPR
	 S_CMPR1:  {regno,DrReg,LdB,next_state}={rt,1'b1,1'b1,S_CMPR2};
	 S_CMPR2:  {ALUstate,ALUfunc,DrALU,regno,WrReg,next_state}={1'b1,op2,1'b1,rd,1'b1,S_FETCH1};
	 //S_CMPI
	 S_CMPI1:  {DrOff,LdB,next_state}={1'b1,1'b1,S_CMPI2};
	 S_CMPI2:  {ALUstate,ALUfunc,DrALU,regno,WrReg,next_state}={1'b1,op2_t,1'b1,rd,1'b1,S_FETCH1};
	 
	 S_JAL1:	{regno,WrReg,DrPC,next_state}={rd,1'b1,1'b1,S_JAL2};
	 S_JAL2: {ShOff,LdB,next_state}={1'b1,1'b1,S_JAL3};
	 S_JAL3:	{ALUfunc,DrALU,LdPC,next_state}={OP2_ADD,1'b1,1'b1,S_FETCH1};
	 //BCOND
	 S_BCOND1: {regno,DrReg,LdB,next_state}={rt,1'b1,1'b1,S_BCOND2};
	 S_BCOND2: begin
	   {ALUstate,ALUfunc,DrPC,LdA}={1'b1,op2_d,1'b1,1'b1};
		  if(ALUout[0]==1'b1)
		    next_state<=S_BCOND3;
		  else 
		    next_state<=S_FETCH1;
		  end
	 S_BCOND3: {ShOff,LdB,next_state}={1'b1,1'b1,S_BCOND4};
	 S_BCOND4: {ALUfunc,DrALU,LdPC,next_state}={OP2_ADD,1'b1 ,1'b1,S_FETCH1}; 
    default:  next_state=S_ERROR;
    endcase
  end
  always @(posedge clk or posedge reset)
	if(reset)
	  state<=S_FETCH1;
	else
	  state<=next_state;
endmodule

module SXT(IN,OUT);
  parameter IBITS;
  parameter OBITS;
  input  [(IBITS-1):0] IN;
  output [(OBITS-1):0] OUT;
  assign OUT={{(OBITS-IBITS){IN[IBITS-1]}},IN};
endmodule
