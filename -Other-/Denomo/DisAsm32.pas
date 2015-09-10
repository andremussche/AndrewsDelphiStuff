unit DisAsm32;
////////////////////////////////////////////////////////////////////////////////
//
//   Unit        :  DISASM32.PAS (requires DISASM32.RES}
//   Date        :  02.29.2004
//   Conversion  :  Russell Libby
//
//   Description :  This is a Delphi conversion of the libdisasm project, which
//                  is a sub portion of the "bastard" project:
//
//                  http://bastard.sourceforge.net/libdisasm.html
//
//                  The libdisasm project is distributed under an "Artistic
//                  License", and I give full credit to the original authors.
//                  Special thanks also goes out to "mammon_" ;-)
//
//   ---------------------------------------------------------------------------
//
//   The Bastard disassembler is released under the Artistic License. This allows
//   the Bastard to be freely distributed and modified; however it cannot be
//   incorporated into a commercial product -- aside from OS distributions and other
//   collections of user software -- without the express permission of the developer.
//
//   ===========================================================================
//   The "Artistic License"
//   Preamble
//
//   The intent of this document is to state the conditions under which a
//   Package may be copied, such that the Copyright Holder maintains some
//   semblance of artistic control over the development of the package,
//   while giving the users of the package the right to use and distribute
//   the Package in a more-or-less customary fashion, plus the right to make
//   reasonable modifications.
//
//   Definitions:
//
//      "Package" refers to the collection of files distributed by the
//       Copyright Holder, and derivatives of that collection of files
//       created through textual modification.
//
//       "Standard Version" refers to such a Package if it has not been
//       modified, or has been modified in accordance with the wishes
//       of the Copyright Holder.
//
//       "Copyright Holder" is whoever is named in the copyright or
//       copyrights for the package.
//
//       "You" is you, if you're thinking about copying or distributing
//       this Package.
//
//       "Reasonable copying fee" is whatever you can justify on the
//       basis of media cost, duplication charges, time of people involved,
//       and so on.  (You will not be required to justify it to the
//       Copyright Holder, but only to the computing community at large
//       as a market that must bear the fee.)
//
//       "Freely Available" means that no fee is charged for the item
//       itself, though there may be fees involved in handling the item.
//       It also means that recipients of the item may redistribute it
//       under the same conditions they received it.
//
//   1. You may make and give away verbatim copies of the source form of the
//   Standard Version of this Package without restriction, provided that you
//   duplicate all of the original copyright notices and associated disclaimers.
//
//   2. You may apply bug fixes, portability fixes and other modifications
//   derived from the Public Domain or from the Copyright Holder.  A Package
//   modified in such a way shall still be considered the Standard Version.
//
//   3. You may otherwise modify your copy of this Package in any way, provided
//   that you insert a prominent notice in each changed file stating how and
//   when you changed that file, and provided that you do at least ONE of the
//   following:
//
//    a) Place your modifications in the Public Domain or otherwise make them
//    Freely Available, such as by posting said modifications to Usenet or
//    an equivalent medium, or placing the modifications on a major archive
//    site such as ftp.uu.net, or by allowing the Copyright Holder to include
//    your modifications in the Standard Version of the Package.
//
//    b) Use the modified Package only within your corporation or organization.
//
//    c) Rename any non-standard executables so the names do not conflict
//    with standard executables, which must also be provided, and provide
//    a separate manual page for each non-standard executable that clearly
//    documents how it differs from the Standard Version.
//
//    d) Make other distribution arrangements with the Copyright Holder.
//
//   4. You may distribute the programs of this Package in object code or
//   executable form, provided that you do at least ONE of the following:
//
//    a) Distribute a Standard Version of the executables and library files,
//    together with instructions (in the manual page or equivalent) on where
//    to get the Standard Version.
//
//    b) Accompany the distribution with the machine-readable source of
//    the Package with your modifications.
//
//    C) Accompany any non-standard executables with their corresponding
//    Standard Version executables, giving the non-standard executables
//    non-standard names, and clearly documenting the differences in manual
//    pages (or equivalent), together with instructions on where to get
//    the Standard Version.
//
//    D) Make other distribution arrangements with the Copyright Holder.
//
//   5. You may charge a reasonable copying fee for any distribution of this
//   Package.  You may charge any fee you choose for support of this Package.
//   You may not charge a fee for this Package itself.  However,
//   you may distribute this Package in aggregate with other (possibly
//   commercial) programs as part of a larger (possibly commercial) software
//   distribution provided that you do not advertise this Package as a
//   product of your own.
//
//   6. The scripts and library files supplied as input to or produced as
//   output from the programs of this Package do not automatically fall
//   under the copyright of this Package, but belong to whomever generated
//   them, and may be sold commercially, and may be aggregated with this
//   Package.
//
//   7. C or perl subroutines supplied by you and linked into this Package
//   shall not be considered part of this Package.
//
//   8. The name of the Copyright Holder may not be used to endorse or promote
//   products derived from this software without specific prior written permission.
//
//   9. THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
//   IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
//   WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
//
//   The End
//
//   ---------------------------------------------------------------------------
//
//   Notes       :  - Project conversion started 02.22.2004
//                  - Moved the opcode table from record constants in the unit,
//                    over to a resource file. The reason for this was the fact
//                    that Delphi failed to allow one to debug this code when
//                    the constant data size exceeded 64K.
//                  - Conversion finished on 02.26.2004.
//                  - Please note that when disassembling procedures or functions
//                    that have break points set in them while in the Delphi IDE,
//                    you may get some unexpected results. This is due to the way
//                    the debugger works. Behind the scenes it (the Delphi Debugger)
//                    will replace the instruction where the break is located with
//                    an INT 3 instruction. For multi-byte instructions, this
//                    skews the next instruction when disassembling. This is NOT
//                    A BUG with the disassembler!
//
//   References  :  The following are links to references / source code that
//                  proved invaluable when converting this disassembler.
//
//                  Original Source:
//                  http://bastard.sourceforge.net/libdisasm.html
//
//                  Intel Pentium Instruction Set in Windows Help format
//                  http://protools.anticrack.de/files/docs/asmhelp.zip
//
//
//   --- Disassembler Class Object ---
//
//   TDisAsm  = class(TObject)
//
//   The TDisAsm class object is designed as nothing more that a wrapper around
//   base library calls exposed in this unit. This saves the developer from the
//   trouble of having to call DisassembleInit/DisassembleCleanup, and then
//   having to track the address being disassembled, etc...
//
//   Data Types:
//   -------------------
//   TDisAsmInstr      -  The item data type stored in the Instructions property.
//   {
//      Code:          -  libinstr data type that is a high level structure for
//                        the disassembled code instruction. Please review the
//                        libinstr for a further breakdown of fields.
//      Address:       -  Starting pointer address for the instruction.
//      OffsetFromBase -  Number of bytes this instruction is offset from the
//                        start of the disassembled address. For the first
//                        instruction, this offset is zero.
//   }
//
//   Properties:
//   -------------------
//
//      BaseAddress    -  This is the pointer to the memory address that was
//                        disassembled. All instructions are offset from this
//                        starting address.
//      Count          -  Number of instructions in the Instructions[Index] property.
//      Instructions   -  Indexed list of TDisAsmInstr items.
//      Size           -  Total size in bytes of the memory that was disassembled.
//
//   Methods:
//   -------------------
//
//      procedure      Disassemble(Address: Pointer; Size: Cardinal = 0);
//
//      The Disassemble procedure is used to "disassemble" the data in memory,
//      starting at Address, into asm code instructions. This procedure also
//      takes an optional Size value, which if specified, indicates how many
//      bytes of memory should be disassembled. It should be noted that this
//      number is a guideline, and is not taken to indicate the max number of
//      bytes. For example, if a Size of 20 is specified, and a 2 byte instruction
//      starts at the 19th byte, then 21 total bytes will be read. If the Size is
//      not specied (defaults to zero), then the Disassemble routine will execute
//      until a RET instruction is hit. So, if you plan on disassembling memory
//      that MAY NOT be valid, it is recommended that you set the size, and make
//      sure some padding space is allowed for.
//
////////////////////////////////////////////////////////////////////////////////
interface

uses
  Windows, SysUtils, Classes;

////////////////////////////////////////////////////////////////////////////////
// Table Data Resource
////////////////////////////////////////////////////////////////////////////////
{$R DISASM32.RES}

////////////////////////////////////////////////////////////////////////////////
// Misc data types
////////////////////////////////////////////////////////////////////////////////
type
  PCardinal         =  ^Cardinal;
  TExpArray         =  Array [0..2] of PCardinal;

////////////////////////////////////////////////////////////////////////////////
// Stripped down combination Bastard.h and bdb.h for libdisasm
////////////////////////////////////////////////////////////////////////////////
type
  Paddr_exp         =  ^addr_exp;
  addr_exp          =  packed record
     scale:         Integer;
     index:         Integer;
     base:          Integer;
     disp:          Integer;
     flags:         Integer;
     used:          Integer;
  end;

type
  Pcode             =  ^Code;
  code              =  packed record
     rva:           Integer;
     func:          Integer;
     mnemonic:      Array [0..15] of AnsiChar;
     dest:          Integer;
     src:           Integer;
     aux:           Integer;
     mnemType:      Cardinal;
     destType:      Cardinal;
     srcType:       Cardinal;
     auxType:       Cardinal;
     destConst:     Cardinal;
     srcConst:      Cardinal;
     auxConst:      Cardinal;
  end;

type
  Pcode_effect      =  ^code_effect;
  code_effect       =  packed record
     id:            Cardinal;
     rva:           Cardinal;
     reg:           Integer;
     change:        Integer;
  end;

// Copied from extensions/arch.h in the bastard
const
  BIG_ENDIAN_ORD    =  0;
  LITTLE_ENDIAN_ORD =  1;

// Disassembler options
const
  IGNORE_NULLS      =  $01;        // Don't disassemble sequences of > 4 NULLs
  LEGACY_MODE       =  $02;        // E.g. 16-bit on Intel

// Operand and instruction types - Permissions
const
  OP_R              =  $01;        // Operand is READ
  OP_W              =  $02;        // Operand is WRITTEN
  OP_X              =  $04;        // Operand is EXECUTED

// Operand and instruction types - Types
const
  OP_UNK            =  $0000;      // Unknown operand
  OP_REG            =  $0100;      // Register
  OP_IMM            =  $0200;      // Immediate value
  OP_REL            =  $0300;      // Relative Address [offset from IP]
  OP_ADDR           =  $0400;      // Absolute Address
  OP_EXPR           =  $0500;      // Address Expression [e.g. SIB byte]
  OP_PTR            =  $0600;      // Operand is an Address containing a Pointer
  OP_OFF            =  $0700;      // Operand is an offset from a seg/selector

// Operand and instruction types - Modifiers
const
  OP_SIGNED         =  $00001000;  // Operand is signed
  OP_STRING         =  $00002000;  // Operand a string
  OP_CONST          =  $00004000;  // Operand is a constant
  OP_EXTRASEG       =  $00010000;  // Seg overrides
  OP_CODESEG        =  $00020000;
  OP_STACKSEG       =  $00030000;
  OP_DATASEG        =  $00040000;
  OP_DATA1SEG       =  $00050000;
  OP_DATA2SEG       =  $00060000;

// Operand and instruction types - Size
const
  OP_BYTE           =  $00100000;  // Operand is 8 bits/1 byte
  OP_HWORD          =  $00200000;  // Operand is .5 mach word (Intel 16 bits)
  OP_WORD           =  $00300000;  // Operand is 1 machine word (Intel 32 bits)
  OP_DWORD          =  $00400000;  // Operand is 2 mach words (Intel 64 bits)
  OP_QWORD          =  $00500000;  // Operand is 4 mach words (Intel 128 bits)
  OP_SREAL          =  $00600000;  // Operand is 32 bits/4 bytes
  OP_DREAL          =  $00700000;  // Operand is 64 bits/8 bytes
  OP_XREAL          =  $00800000;  // Operand is 40 bits/10 bytes
  OP_BCD            =  $00900000;  // Operand is 40 bits/10 bytes
  OP_SIMD           =  $00A00000;  // Operand is 128 bits/16 bytes
  OP_FPENV          =  $00B00000;  // Operand is 224 bits/28 bytes

// Operand masks
const
  OP_PERM_MASK      =  $00000007;  // Perms are NOT mutually exclusive
  OP_TYPE_MASK      =  $00000F00;  // Types are mututally exclusive
  OP_MOD_MASK       =  $000FF000;  // Mods are NOT mutually exclusive
  OP_SEG_MASK       =  $000F0000;  // Segs are NOT mutually exclusive
  OP_SIZE_MASK      =  $00F00000;  // Sizes are mutually exclusive
  OP_REG_MASK       =  $0000FFFF;  // Lower WORD is register ID
  OP_REGTBL_MASK    =  $FFFF0000;  // Higher word is register type [gen/dbg]

// Instruction types [groups]
const
  INS_EXEC          =  $1000;
  INS_ARITH         =  $2000;
  INS_LOGIC         =  $3000;
  INS_STACK         =  $4000;
  INS_COND          =  $5000;
  INS_LOAD          =  $6000;
  INS_ARRAY         =  $7000;
  INS_BIT           =  $8000;
  INS_FLAG          =  $9000;
  INS_FPU           =  $A000;
  INS_TRAPS         =  $D000;
  INS_SYSTEM        =  $E000;
  INS_OTHER         =  $F000;

// INS_EXEC group
const
  INS_BRANCH        =  INS_EXEC or $01;     // Unconditional branch
  INS_BRANCHCC      =  INS_EXEC or $02;     // Conditional branch
  INS_CALL          =  INS_EXEC or $03;     // Jump to subroutine
  INS_CALLCC        =  INS_EXEC or $04;     // Jump to subroutine
  INS_RET           =  INS_EXEC or $05;     // Return from subroutine
  INS_LOOP          =  INS_EXEC or $06;     // Loop to local label

// INS_ARITH group
const
  INS_ADD           =  INS_ARITH or $01;
  INS_SUB           =  INS_ARITH or $02;
  INS_MUL           =  INS_ARITH or $03;
  INS_DIV           =  INS_ARITH or $04;
  INS_INC           =  INS_ARITH or $05;    // Increment
  INS_DEC           =  INS_ARITH or $06;    // Decrement
  INS_SHL           =  INS_ARITH or $07;    // Shift right
  INS_SHR           =  INS_ARITH or $08;    // Shift left
  INS_ROL           =  INS_ARITH or $09;    // Rotate left
  INS_ROR           =  INS_ARITH or $0A;    // Rotate right

// INS_LOGIC group
const
  INS_AND           =  INS_LOGIC or $01;
  INS_OR            =  INS_LOGIC or $02;
  INS_XOR           =  INS_LOGIC or $03;
  INS_NOT           =  INS_LOGIC or $04;
  INS_NEG           =  INS_LOGIC or $05;

// INS_STACK group
const
  INS_PUSH          =  INS_STACK or $01;
  INS_POP           =  INS_STACK or $02;
  INS_PUSHREGS      =  INS_STACK or $03;    // Push register context
  INS_POPREGS       =  INS_STACK or $04;    // Pop register context
  INS_PUSHFLAGS     =  INS_STACK or $05;    // Push all flags
  INS_POPFLAGS      =  INS_STACK or $06;    // Pop all flags
  INS_ENTER         =  INS_STACK or $07;    // Enter stack frame
  INS_LEAVE         =  INS_STACK or $08;    // Leave stack frame

// INS_COND group
const
  INS_TEST          =  INS_COND or $01;
  INS_CMP           =  INS_COND or $02;

// INS_LOAD group
const
  INS_MOV           =  INS_LOAD or $01;
  INS_MOVCC         =  INS_LOAD or $02;
  INS_XCHG          =  INS_LOAD or $03;
  INS_XCHGCC        =  INS_LOAD or $04;

// INS_ARRAY group
const
  INS_STRCMP        =  INS_ARRAY or $01;
  INS_STRLOAD       =  INS_ARRAY or $02;
  INS_STRMOV        =  INS_ARRAY or $03;
  INS_STRSTOR       =  INS_ARRAY or $04;
  INS_XLAT          =  INS_ARRAY or $05;

// INS_BIT group
const
  INS_BITTEST       =  INS_BIT or $01;
  INS_BITSET        =  INS_BIT or $02;
  INS_BITCLR        =  INS_BIT or $03;

// INS_FLAG group
const
  INS_CLEARCF       =  INS_FLAG or $01;  // Clear Carry flag
  INS_CLEARZF       =  INS_FLAG or $02;  // Clear Zero flag
  INS_CLEAROF       =  INS_FLAG or $03;  // Clear Overflow flag
  INS_CLEARDF       =  INS_FLAG or $04;  // Clear Direction flag
  INS_CLEARSF       =  INS_FLAG or $05;  // Clear Sign flag
  INS_CLEARPF       =  INS_FLAG or $06;  // Clear Parity flag
  INS_SETCF         =  INS_FLAG or $07;
  INS_SETZF         =  INS_FLAG or $08;
  INS_SETOF         =  INS_FLAG or $09;
  INS_SETDF         =  INS_FLAG or $0A;
  INS_SETSF         =  INS_FLAG or $0B;
  INS_SETPF         =  INS_FLAG or $0C;
  INS_TOGCF         =  INS_FLAG or $10;  // Toggle
  INS_TOGZF         =  INS_FLAG or $20;
  INS_TOGOF         =  INS_FLAG or $30;
  INS_TOGDF         =  INS_FLAG or $40;
  INS_TOGSF         =  INS_FLAG or $50;
  INS_TOGPF         =  INS_FLAG or $60;

// INS_FPU

// INS_TRAP
const
  INS_TRAP          =  INS_TRAPS or $01;    // Generate trap
  INS_TRAPCC        =  INS_TRAPS or $02;    // Conditional trap gen
  INS_TRET          =  INS_TRAPS or $03;    // Return from trap
  INS_BOUNDS        =  INS_TRAPS or $04;    // Gen bounds trap
  INS_DEBUG         =  INS_TRAPS or $05;    // Gen breakpoint trap
  INS_TRACE         =  INS_TRAPS or $06;    // Gen single step trap
  INS_INVALIDOP     =  INS_TRAPS or $07;    // Gen invalid instruction
  INS_OFLOW         =  INS_TRAPS or $08;    // Gen overflow trap

// INS_SYSTEM
const
  INS_HALT          =  INS_SYSTEM or $01;      // Halt machine
  INS_IN            =  INS_SYSTEM or $02;      // Input from port
  INS_OUT           =  INS_SYSTEM or $03;      // Output to port
  INS_CPUID         =  INS_SYSTEM or $04;      // Identify cpu

// INS_OTHER
const
  INS_NOP           =  INS_OTHER or $01;
  INS_BCDCONV       =  INS_OTHER or $02;     // Convert to/from BCD
  INS_SZCONV        =  INS_OTHER or $03;     // Convert size of operand

// Instruction size
const
  INS_BYTE          =  $010000;             // Operand is  8 bits/1 byte
  INS_WORD          =  $020000;             // Operand is 16 bits/2 bytes
  INS_DWORD         =  $040000;             // Operand is 32 bits/4 bytes
  INS_QWORD         =  $080000;             // Operand is 64 bits/8 bytes

// Instruction modifiers
const
  INS_REPZ          =  $00100000;
  INS_REPNZ         =  $00200000;
  INS_LOCK          =  $00400000;           // Lock bus
  INS_DELAY         =  $00800000;           // Branch delay slot

// Masks
const
  INS_TYPE_MASK     =  $0000FFFF;
  INS_GROUP_MASK    =  $00001000;
  INS_SIZE_MASK     =  $000F0000;
  INS_MOD_MASK      =  $0FF00000;

// Code patterns
const
  FUNCTION_PROLOGUE =  $0001;
  FUNCTION_EPILOGUE =  $0002;

// These could reuse OP types, but those types are too general...
const
  ADDEXP_SCALE_MASK =  $000000FF;
  ADDEXP_INDEX_MASK =  $0000FF00;
  ADDEXP_BASE_MASK  =  $00FF0000;
  ADDEXP_DISP_MASK  =  $FF000000;
  ADDEXP_SCALE_OFFSET  =  0;
  ADDEXP_INDEX_OFFSET  =  8;
  ADDEXP_BASE_OFFSET   =  16;
  ADDEXP_DISP_OFFSET   =  24;
  ADDREXP_BYTE      =  $01;
  ADDREXP_WORD      =  $02;
  ADDREXP_DWORD     =  $03;
  ADDREXP_QWORD     =  $04;
  ADDREXP_REG       =  $10; // 0x00 implies non-register

// Macro conversions
function   AddrExp_ScaleType(x: Integer): Integer;
function   AddrExp_IndexType(x: Integer): Integer;
function   AddrExp_BaseType(x: Integer): Integer;
function   AddrExp_DispType(x: Integer): Integer;

// Pointless defines for fixing i386.c
const
  CODE_RVA          =  0;

////////////////////////////////////////////////////////////////////////////////
// VM.h Conversion
////////////////////////////////////////////////////////////////////////////////
type
  PRegTblEntry      =  ^REGTBL_ENTRY;
  REGTBL_ENTRY      =  packed record
     size:          Integer;
     _type:         Integer;
     data:          Pointer;
     mnemonic:      Array [0..7] of Char;
  end;
  TRegTblArray      =  Array [0..32677] of REGTBL_ENTRY;
  PRegTblArray      =  ^TRegTblArray;

// Register types
const
  REG_GENERAL       =  $00000001;        // General-purpose
  REG_INPUT         =  $00000002;       // Incoming args
  REG_OUTPUT        =  $00000004;       // Args to subroutines
  REG_LOCAL         =  $00000008;       // Local vars
  REG_FPU           =  $00000010;       // FPU reg
  REG_SEG           =  $00000020;       // Segment reg
  REG_SIMD          =  $00000040;       // Simd/mmx stuff
  REG_SYS           =  $00000080;       // CPU/OS internal reg
  REG_SP            =  $00001000;        // Stack pointer
  REG_FP            =  $00002000;        // Frame pointer
  REG_PC            =  $00004000;        // Program counter
  REG_RETADD        =  $00008000;        // Return address
  REG_CC            =  $00010000;        // Condition code
  REG_ZERO          =  $00020000;        // /dev/null register
  REG_RET           =  $00040000;        // Return value
  REG_ASRC          =  $00100000;        // Array source
  REG_ADEST         =  $00200000;        // Array dest
  REG_ACNT          =  $00400000;        // Array length/counter

////////////////////////////////////////////////////////////////////////////////
// Extension.h Conversion
////////////////////////////////////////////////////////////////////////////////
type
  ext_init_fn       =  procedure(p: Pointer); stdcall;
  ext_clean_fn      =  procedure; stdcall;

type
  PExtension        =  ^EXTENSION;
  EXTENSION         =  packed record
     filename:      PChar;
     flags:         Integer;
     lib:           Pointer;
     fn_init:       ext_init_fn;
     fn_cleanup:    ext_clean_fn;
  end;

// Predef's for use with dynamic loading
type
  disfunc_fn        =  function(p1: PChar; p2: Byte; p3: Pcode; p4: Integer): Integer; stdcall;
  getcode_fn        =  function(var p1: Pcode): Integer; stdcall;

type
  PExtArch          =  ^EXT__ARCH;
  EXT__ARCH         =  packed record     // Disassembler information
     ext:           EXTENSION;
     options:       Integer;
     cpu_hi:        Integer;
     cpu_lo:        Integer;
     endian:        Byte;
     sz_addr:       Byte;
     sz_oper:       Byte;
     sz_inst:       Byte;
     sz_byte:       Byte;
     sz_word:       Byte;
     sz_dword:      Byte;
     SP:            Integer;
     IP:            Integer;
     FP:            Integer;
     reg_gen:       Integer;
     reg_seg:       Integer;
     reg_fp:        Integer;
     reg_in:        Integer;
     reg_out:       Integer;
     reg_table:     PRegTblArray;
     sz_regtable:   Integer;
     reg_storage:   PChar;
     fn_disasm_addr:disfunc_fn;
     fn_prologue:   getcode_fn;
     fn_epilogue:   getcode_fn;
  end;

////////////////////////////////////////////////////////////////////////////////
// Libdis.h Conversion
////////////////////////////////////////////////////////////////////////////////

// Formats
const
  NATIVE_SYNTAX     =  $00;
  INTEL_SYNTAX      =  $01;
  ATT_SYNTAX        =  $02;

type
  PlibInstr         =  ^libinstr;
  libinstr          =  packed record
     szText:        Array [0..71] of Char;
     mnemonic:      Array [0..15] of AnsiChar;
     mnemType:      Integer;
     destType:      Integer;
     destVal:       Integer;
     srcType:       Integer;
     srcVal:        Integer;
     auxType:       Integer;
     auxVal:        Integer;
     size:          Integer;
  end;
  TLibInstr         =  libinstr;

// Macro conversions
function   cpu_endian(ext_arch: EXT__ARCH): Byte;
function   cpu_addr_size(ext_arch: EXT__ARCH): Byte;
function   cpu_op_size(ext_arch: EXT__ARCH): Byte;
function   cpu_word_size(ext_arch: EXT__ARCH): Byte;
function   cpu_inst_size(ext_arch: EXT__ARCH): Byte;
function   cpu_sp(ext_arch: EXT__ARCH): Integer;
function   cpu_fp(ext_arch: EXT__ARCH): Integer;
function   cpu_ip(ext_arch: EXT__ARCH): Integer;

////////////////////////////////////////////////////////////////////////////////
// I386.h Conversion
////////////////////////////////////////////////////////////////////////////////

// Opcode tables
const
  x86_MAIN          =  0;
  x86_0F            =  1;
  x86_80            =  2;

const
  REG_DWORD_OFFSET  =  0;
  REG_WORD_OFFSET   =  1 * 8;
  REG_BYTE_OFFSET   =  2 * 8;
  REG_MMX_OFFSET    =  3 * 8;
  REG_SIMD_OFFSET   =  4 * 8;
  REG_DEBUG_OFFSET  =  5 * 8;
  REG_CTRL_OFFSET   =  6 * 8;
  REG_TEST_OFFSET   =  7 * 8;
  REG_SEG_OFFSET    =  8 * 8;
  REG_FPU_OFFSET    =  9 * 8;
  REG_FLAGS_INDEX   =  10 * 8;
  REG_FPCTRL_INDEX  =  10 * 8 + 1;
  REG_FPSTATUS_INDEX=  10 * 8 + 2;
  REG_FPTAG_INDEX   =  10 * 8 + 3;
  REG_EIP_INDEX     =  10 * 8 + 4;
  REG_IP_INDEX      =  10 * 8 + 5;

const
  REG_DWORD_SIZE    =  4;
  REG_WORD_SIZE     =  2;
  REG_BYTE_SIZE     =  1;
  REG_MMX_SIZE      =  4;
  REG_SIMD_SIZE     =  4;
  REG_DEBUG_SIZE    =  4;
  REG_CTRL_SIZE     =  4;
  REG_TEST_SIZE     =  4;
  REG_SEG_SIZE      =  2;
  REG_FPU_SIZE      =  10;
  REG_FLAGS_SIZE    =  4;
  REG_FPCTRL_SIZE   =  2;
  REG_FPSTATUS_SIZE =  2;
  REG_FPTAG_SIZE    =  2;
  REG_EIP_SIZE      =  4;
  REG_IP_SIZE       =  2;

// Add TR LDTR [6 bytes] and IDTR GDTR (4bytes)

// The following dictate what ISAs to support - for now these do nothing
const
  ISA_8086          =  $00000010;
  ISA_80286         =  $00000020;
  ISA_80386         =  $00000040;
  ISA_80486         =  $00000080;
  ISA_PENTIUM       =  $00000100;
  ISA_PENTIUM_2     =  $00000200;
  ISA_PENTIUM_3     =  $00000400;
  ISA_PENTIUM_4     =  $00000800;
  ISA_K6            =  $00001000;
  ISA_K7            =  $00002000;
  ISA_ATHLON        =  $00004000;
  ISA_SIMD          =  $00010000;
  ISA_MMX           =  $00020000;
  ISA_3DNOW         =  $00040000;

////////////////////////////////////////////////////////////////////////////////
// I386_opcode.h Conversion
////////////////////////////////////////////////////////////////////////////////

// Instruction structure - used for reading opcode table
type
  Pinstr            =  ^instr;
  instr             =  packed record
     table:         Integer;
     mnemFlg:       Cardinal;
     destFlg:       Integer;
     srcFlg:        Integer;
     auxFlg:        Integer;
     cpu:           Integer;
     mnemonic:      Array [0..15] of AnsiChar;
     dest:          Integer;
     src:           Integer;
     aux:           Integer;
  end;
  TInstrArray       =  Array [0..255] of instr;
  PInstrArray       =  ^TInstrArray;

const
  MAX_INSTRUCTION_SIZE    =  20;

// Opcode Prefixes
const
  INSTR_PREFIX      =  $F0000000;

// Prefixes, same order as in the manual
const
  PREFIX_LOCK       =  $00100000;
  PREFIX_REPNZ      =  $00200000;
  PREFIX_REPZ       =  $00400000;
  PREFIX_REP        =  $00800000;
  PREFIX_REP_SIMD   =  $01000000;
  PREFIX_OP_SIZE    =  $02000000;
  PREFIX_ADDR_SIZE  =  $04000000;
  PREFIX_SIMD       =  $08000000;
  PREFIX_CS         =  $10000000;
  PREFIX_SS         =  $20000000;
  PREFIX_DS         =  $30000000;
  PREFIX_ES         =  $40000000;
  PREFIX_FS         =  $50000000;
  PREFIX_GS         =  $60000000;
  PREFIX_REG_MASK   =  $F0000000;

// The Flags That Time Forgot
const
  ARG_NONE          =  0;
  cpu_8086          =  $00001000;
  cpu_80286         =  $00002000;
  cpu_80386         =  $00003000;
  cpu_80387         =  $00004000;
  cpu_80486         =  $00005000;
  cpu_PENTIUM       =  $00006000;
  cpu_PENTPRO       =  $00007000;
  cpu_PENTMMX       =  $00008000;
  cpu_PENTIUM2      =  $00009000;

//Operand classifications, per dislib.h, go to 0x0900
const
  OPFLAGS_MASK      =  $0000FFFF;

// Operand Addressing Methods, per intel manual
const
  ADDRMETH_MASK     =  $00FF0000;

const
  ADDRMETH_A        =  $00010000;
  ADDRMETH_C        =  $00020000;
  ADDRMETH_D        =  $00030000;
  ADDRMETH_E        =  $00040000;
  ADDRMETH_F        =  $00050000;
  ADDRMETH_G        =  $00060000;
  ADDRMETH_I        =  $00070000;
  ADDRMETH_J        =  $00080000;
  ADDRMETH_M        =  $00090000;
  ADDRMETH_O        =  $000A0000;
  ADDRMETH_P        =  $000B0000;
  ADDRMETH_Q        =  $000C0000;
  ADDRMETH_R        =  $000D0000;
  ADDRMETH_S        =  $000E0000;
  ADDRMETH_T        =  $000F0000;
  ADDRMETH_V        =  $00100000;
  ADDRMETH_W        =  $00110000;
  ADDRMETH_X        =  $00120000;
  ADDRMETH_Y        =  $00130000;

// Operand Size Codings
const
  OP_SIZE_8         =  $00200000;
  OP_SIZE_16        =  $00400000;
  OP_SIZE_32        =  $00800000;

// Operand Types, per intel manual
const
  OPTYPE_MASK       =  $FF000000;

const
  OPTYPE_a          =  $01000000;
  OPTYPE_b          =  $02000000;
  OPTYPE_c          =  $03000000;
  OPTYPE_d          =  $04000000;
  OPTYPE_dq         =  $05000000;
  OPTYPE_p          =  $06000000;
  OPTYPE_pi         =  $07000000;
  OPTYPE_ps         =  $08000000;
  OPTYPE_q          =  $09000000;
  OPTYPE_s          =  $0A000000;
  OPTYPE_ss         =  $0B000000;
  OPTYPE_si         =  $0C000000;
  OPTYPE_v          =  $0D000000;
  OPTYPE_w          =  $0E000000;
  OPTYPE_m          =  $0F000000;     // To handle LEA

// Ones added for FPU instructions
const
  OPTYPE_fs        =  $10000000;     // Pointer to single-real
  OPTYPE_fd        =  $20000000;     // Pointer to double real
  OPTYPE_fe        =  $30000000;     // Pointer to extended real
  OPTYPE_fb        =  $40000000;     // Pointer to packed BCD
  OPTYPE_fv        =  $50000000;     // Pointer to FPU env: 14/28-bytes


// ModR/M, SIB - Convenience flags
const
  MODRM_EA          =  1;
  MODRM_reg         =  2;

// ModR/M flags
const
  MODRM_RM_SIB      =  $04;
  MODRM_RM_NOREG    =  $05;
  // if (MODRM.MOD_NODISP && MODRM.RM_NOREG) then just disp32
  MODRM_MOD_NODISP  =  $00;
  MODRM_MOD_DISP8   =  $01;
  MODRM_MOD_DISP32  =  $02;
  MODRM_MOD_NOEA    =  $03;

// SIB flags
const
  SIB_INDEX_NONE    =  $04;
  SIB_BASE_EBP      =  $05;
  SIB_SCALE_NOBASE  =  $00;

// Convenience struct for modR/M bitfield
type
  PmodRMbyte        =  ^modRM_byte;
  modRM_byte        =  packed record
     _mod:          Integer;
     reg:           Integer;
     rm:            Integer;
  end;

const
  cmodRM_byte:      modRM_byte  =  (_mod: 2; reg: 3; rm: 3);

// Convenience struct for SIB bitfield
type
  PSIBbyte          =  ^SIB_byte;
  SIB_byte          =  packed record
     scale:         Integer;
     index:         Integer;
     base:          Integer;
  end;

const
  cSIB_byte:        SIB_byte    =  (scale: 2; index: 3; base: 3);

// Convenience struct for opcode tables
type
  Px86table         =  ^x86_table;
  x86_table         =  packed record
     table:         PInstrArray;
     shift:         Byte;
     mask:          Byte;
     minlim:        Byte;
     maxlim:        Byte;
  end;
  asmtable          =  x86_table;


////////////////////////////////////////////////////////////////////////////////
// I386.opcode.map Conversion
////////////////////////////////////////////////////////////////////////////////
var
  tbl_Main:         Array [0..255] of instr;
  tbl_0F:           Array [0..255] of instr;
  tbl_0F00:         Array [0..7] of instr;
  tbl_0F01:         Array [0..7] of instr;
  tbl_0F18:         Array [0..7] of instr;
  tbl_0F71:         Array [0..7] of instr;
  tbl_0F72:         Array [0..7] of instr;
  tbl_0F73:         Array [0..1] of instr;
  tbl_0FAE:         Array [0..4] of instr;
  tbl_0FBA:         Array [0..3] of instr;
  tbl_0FC7:         Array [0..0] of instr;
  tbl_80:           Array [0..7] of instr;
  tbl_81:           Array [0..7] of instr;
  tbl_82:           Array [0..7] of instr;
  tbl_83:           Array [0..7] of instr;
  tbl_C0:           Array [0..7] of instr;
  tbl_C1:           Array [0..7] of instr;
  tbl_D0:           Array [0..7] of instr;
  tbl_D1:           Array [0..7] of instr;
  tbl_D2:           Array [0..7] of instr;
  tbl_D3:           Array [0..7] of instr;
  tbl_F6:           Array [0..7] of instr;
  tbl_F7:           Array [0..7] of instr;
  tbl_FE:           Array [0..1] of instr;
  tbl_FF:           Array [0..7] of instr;
  tbl_fpuD8_00BF:   Array [0..7] of instr;
  tbl_fpuD8_rest:   Array [0..63] of instr;
  tbl_fpuD9_00BF:   Array [0..7] of instr;
  tbl_fpuD9_rest:   Array [0..63] of instr;
  tbl_fpuDA_00BF:   Array [0..7] of instr;
  tbl_fpuDA_rest:   Array [0..63] of instr;
  tbl_fpuDB_00BF:   Array [0..7] of instr;
  tbl_fpuDB_rest:   Array [0..63] of instr;
  tbl_fpuDC_00BF:   Array [0..7] of instr;
  tbl_fpuDC_rest:   Array [0..63] of instr;
  tbl_fpuDD_00BF:   Array [0..7] of instr;
  tbl_fpuDD_rest:   Array [0..63] of instr;
  tbl_fpuDE_00BF:   Array [0..7] of instr;
  tbl_fpuDE_rest:   Array [0..63] of instr;
  tbl_fpuDF_00BF:   Array [0..7] of instr;
  tbl_fpuDF_rest:   Array [0..63] of instr;

// Table loading information
type
  TTblItem          =  packed record
     N:             PChar;
     P:             Pointer;
     S:             Cardinal;
  end;

// Table array used for loading data
const
  tblLoad:          Array [0..40] of TTblItem =
  ((N: 'tbl_Main'; P: @tbl_Main; S: SizeOf(tbl_Main)),
   (N: 'tbl_0F'; P: @tbl_0F; S: SizeOf(tbl_0F)),
   (N: 'tbl_0F00'; P: @tbl_0F00; S: SizeOf(tbl_0F00)),
   (N: 'tbl_0F01'; P: @tbl_0F01; S: SizeOf(tbl_0F01)),
   (N: 'tbl_0F18'; P: @tbl_0F18; S: SizeOf(tbl_0F18)),
   (N: 'tbl_0F71'; P: @tbl_0F71; S: SizeOf(tbl_0F71)),
   (N: 'tbl_0F72'; P: @tbl_0F72; S: SizeOf(tbl_0F72)),
   (N: 'tbl_0F73'; P: @tbl_0F73; S: SizeOf(tbl_0F73)),
   (N: 'tbl_0FAE'; P: @tbl_0FAE; S: SizeOf(tbl_0FAE)),
   (N: 'tbl_0FBA'; P: @tbl_0FBA; S: SizeOf(tbl_0FBA)),
   (N: 'tbl_0FC7'; P: @tbl_0FC7; S: SizeOf(tbl_0FC7)),
   (N: 'tbl_80'; P: @tbl_80; S: SizeOf(tbl_80)),
   (N: 'tbl_81'; P: @tbl_81; S: SizeOf(tbl_81)),
   (N: 'tbl_82'; P: @tbl_82; S: SizeOf(tbl_82)),
   (N: 'tbl_83'; P: @tbl_83; S: SizeOf(tbl_83)),
   (N: 'tbl_C0'; P: @tbl_C0; S: SizeOf(tbl_C0)),
   (N: 'tbl_C1'; P: @tbl_C1; S: SizeOf(tbl_C1)),
   (N: 'tbl_D0'; P: @tbl_D0; S: SizeOf(tbl_D0)),
   (N: 'tbl_D1'; P: @tbl_D1; S: SizeOf(tbl_D1)),
   (N: 'tbl_D2'; P: @tbl_D2; S: SizeOf(tbl_D2)),
   (N: 'tbl_D3'; P: @tbl_D3; S: SizeOf(tbl_D3)),
   (N: 'tbl_F6'; P: @tbl_F6; S: SizeOf(tbl_F6)),
   (N: 'tbl_F7'; P: @tbl_F7; S: SizeOf(tbl_F7)),
   (N: 'tbl_FE'; P: @tbl_FE; S: SizeOf(tbl_FE)),
   (N: 'tbl_FF'; P: @tbl_FF; S: SizeOf(tbl_FF)),
   (N: 'tbl_fpuD8_00BF'; P: @tbl_fpuD8_00BF; S: SizeOf(tbl_fpuD8_00BF)),
   (N: 'tbl_fpuD8_rest'; P: @tbl_fpuD8_rest; S: SizeOf(tbl_fpuD8_rest)),
   (N: 'tbl_fpuD9_00BF'; P: @tbl_fpuD9_00BF; S: SizeOf(tbl_fpuD9_00BF)),
   (N: 'tbl_fpuD9_rest'; P: @tbl_fpuD9_rest; S: SizeOf(tbl_fpuD9_rest)),
   (N: 'tbl_fpuDA_00BF'; P: @tbl_fpuDA_00BF; S: SizeOf(tbl_fpuDA_00BF)),
   (N: 'tbl_fpuDA_rest'; P: @tbl_fpuDA_rest; S: SizeOf(tbl_fpuDA_rest)),
   (N: 'tbl_fpuDB_00BF'; P: @tbl_fpuDB_00BF; S: SizeOf(tbl_fpuDB_00BF)),
   (N: 'tbl_fpuDB_rest'; P: @tbl_fpuDB_rest; S: SizeOf(tbl_fpuDB_rest)),
   (N: 'tbl_fpuDC_00BF'; P: @tbl_fpuDC_00BF; S: SizeOf(tbl_fpuDC_00BF)),
   (N: 'tbl_fpuDC_rest'; P: @tbl_fpuDC_rest; S: SizeOf(tbl_fpuDC_rest)),
   (N: 'tbl_fpuDD_00BF'; P: @tbl_fpuDD_00BF; S: SizeOf(tbl_fpuDD_00BF)),
   (N: 'tbl_fpuDD_rest'; P: @tbl_fpuDD_rest; S: SizeOf(tbl_fpuDD_rest)),
   (N: 'tbl_fpuDE_00BF'; P: @tbl_fpuDE_00BF; S: SizeOf(tbl_fpuDE_00BF)),
   (N: 'tbl_fpuDE_rest'; P: @tbl_fpuDE_rest; S: SizeOf(tbl_fpuDE_rest)),
   (N: 'tbl_fpuDF_00BF'; P: @tbl_fpuDF_00BF; S: SizeOf(tbl_fpuDF_00BF)),
   (N: 'tbl_fpuDF_rest'; P: @tbl_fpuDF_rest; S: SizeOf(tbl_fpuDF_rest)));

// Asm tables
const
  tables86:      Array [0..41] of asmtable =
  ((table: @tbl_Main; shift: 0; mask: $FF; minlim: 0; maxlim: $FF),
   (table: @tbl_0F; shift: 0; mask:  $FF; minlim: 0; maxlim:  $FF),
   (table: @tbl_80; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_81; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_82; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_83; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_C0; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_C1; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_D0; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_D1; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_D2; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_D3; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_F6; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_F7; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_FE; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_FF; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0F00; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0F01; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0F18; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0F71; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0F72; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0F73; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0FAE; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0FBA; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0FC7; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_0FC7; shift: 3; mask: $07; minlim: 0; maxlim: $FF),
   (table: @tbl_fpuD8_00BF; shift: 3; mask: $07; minlim: 0; maxlim: $BF),
   (table: @tbl_fpuD8_rest; shift: 0; mask: $FF; minlim: $C0; maxlim: $FF),
   (table: @tbl_fpuD9_00BF; shift: 3; mask: $07; minlim: 0; maxlim: $BF),
   (table: @tbl_fpuD9_rest; shift: 0; mask: $FF; minlim: $C0; maxlim: $FF),
   (table: @tbl_fpuDA_00BF; shift: 3; mask: $07; minlim: 0; maxlim: $BF),
   (table: @tbl_fpuDA_rest; shift: 0; mask: $FF; minlim: $C0; maxlim: $FF),
   (table: @tbl_fpuDB_00BF; shift: 3; mask: $07; minlim: 0; maxlim: $BF),
   (table: @tbl_fpuDB_rest; shift: 0; mask: $FF; minlim: $C0; maxlim: $FF),
   (table: @tbl_fpuDC_00BF; shift: 3; mask: $07; minlim: 0; maxlim: $BF),
   (table: @tbl_fpuDC_rest; shift: 0; mask: $FF; minlim: $C0; maxlim: $FF),
   (table: @tbl_fpuDD_00BF; shift: 3; mask: $07; minlim: 0; maxlim: $BF),
   (table: @tbl_fpuDD_rest; shift: 0; mask: $FF; minlim: $C0; maxlim: $FF),
   (table: @tbl_fpuDE_00BF; shift: 3; mask: $07; minlim: 0; maxlim: $BF),
   (table: @tbl_fpuDE_rest; shift: 0; mask: $FF; minlim: $C0; maxlim: $FF),
   (table: @tbl_fpuDF_00BF; shift: 3; mask: $07; minlim: 0; maxlim: $BF),
   (table: @tbl_fpuDF_rest; shift: 0; mask: $FF; minlim: $C0; maxlim: $FF));

// These are for inclusion into i386.o - they are extern'ed in i386_opcode.h
const
  prefix_table:     Array [0..12, 0..1] of Integer =
  (($F0, PREFIX_LOCK),
   ($F2, PREFIX_REPNZ),
   ($F3, PREFIX_REP),
   ($2E, PREFIX_CS),
   ($36, PREFIX_SS),
   ($3E, PREFIX_DS),
   ($26, PREFIX_ES),
   ($64, PREFIX_FS),
   ($65, PREFIX_GS),
   ($66, PREFIX_OP_SIZE),
   ($67, PREFIX_ADDR_SIZE),
   ($0F, PREFIX_SIMD ),      // This is not really true: 0F is 2-byte escape
   (0, 0));

const
  reg_gen_type:     Array [0..7] of Integer =
  (REG_GENERAL or REG_RET,
   REG_GENERAL or REG_ACNT,           // ecx
   REG_GENERAL,                       // edx
   REG_GENERAL,                       // ebx
   REG_GENERAL or REG_SP,             // esp
   REG_GENERAL or REG_FP,             // ebp
   REG_GENERAL or REG_ASRC,           // esi
   REG_GENERAL or REG_ADEST);         // edi

const
  rg_dword:         Array [0..7] of PChar = ('eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi');
  rg_word:          Array [0..7] of PChar = ('ax', 'cx', 'dx', 'bx', 'sp', 'bp', 'si', 'di');
  rg_byte:          Array [0..7] of PChar = ('al', 'cl', 'dl', 'bl', 'ah', 'ch', 'dh', 'bh');
  rg_mmx:           Array [0..7] of PChar = ('mm0', 'mm1', 'mm2', 'mm3', 'mm4', 'mm5', 'mm6', 'mm7');
  rg_simd:          Array [0..7] of PChar = ('xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5', 'xmm6', 'xmm7');
  rg_debug:         Array [0..7] of PChar = ('dr0', 'dr1', 'dr2', 'dr3', 'dr4', 'dr5', 'dr6', 'dr7');
  rg_control:       Array [0..7] of PChar = ('cr0', 'cr1', 'cr2', 'cr3', 'cr4', 'cr5', 'cr6', 'cr7');
  rg_test:          Array [0..7] of PChar = ('tr0', 'tr1', 'tr2', 'tr3', 'tr4', 'tr5', 'tr6', 'tr7');
  rg_seg:           Array [0..7] of PChar = ('es', 'cs', 'ss', 'ds', 'fs', 'gs', ' ', ' ');
  rg_fpu:           Array [0..7] of PChar = ('st(0)', 'st(1)', 'st(2)', 'st(3)', 'st(4)', 'st(5)', 'st(6)', 'st(7)');

const
  modrm_rm:         Array [0..7] of Integer = (0, 1, 2, 3, MODRM_RM_SIB, MODRM_MOD_DISP32, 6, 7);
  modrm_rg:         Array [0..7] of Integer = (0, 1, 2, 3, 4, 5, 6, 7);
  modrm_mod:        Array [0..3] of Integer = (0, MODRM_MOD_DISP8, MODRM_MOD_DISP32, MODRM_MOD_NOEA);
  sib_scl:          Array [0..3] of Integer = (0, 2, 4, 8);
  sib_idx:          Array [0..7] of Integer = (0, 1, 2, 3, SIB_INDEX_NONE, 5, 6, 7);
  sib_bas:          Array [0..7] of Integer = (0, 1, 2, 3, 4, SIB_SCALE_NOBASE, 6, 7);

////////////////////////////////////////////////////////////////////////////////
//   Global settings from libdisasm wrapped into a single structure
////////////////////////////////////////////////////////////////////////////////
type
  PDisAsmStruct     =  ^TDisAsmStruct;
  TDisAsmStruct     =  packed record
     settings:      EXT__ARCH;
     mode16:        Integer;
     expr:          addr_exp;
     exp:           Array [0..2] of addr_exp;
  end;

////////////////////////////////////////////////////////////////////////////////
// Library Disassembly functions
////////////////////////////////////////////////////////////////////////////////
function   DisassembleInit(var Struct: TDisAsmStruct; Options, Format: Integer): Integer;
function   DisassembleCleanup(var Struct: TDisAsmStruct): Integer;
function   DisassembleAddress(var Struct: TDisAsmStruct; Buffer: Pointer; var Instruction: libinstr): Integer;

////////////////////////////////////////////////////////////////////////////////
// Object wrapper for disassembly
////////////////////////////////////////////////////////////////////////////////
type
  // Custom data type to hold the base libinstr as well as the additonal address / offset fields
  PDisAsmInstr      =  ^TDisAsmInstr;
  TDisAsmInstr      =  packed record
     Code:          TLibInstr;
     Address:       Pointer;
     OffsetFromBase:Cardinal;
  end;
  // Disassembler class
  TDisAsm           =  class(TObject)
  private
     // Private declarations
     FSize:         Cardinal;
     FAddress:      Pointer;
     FInstrList:    TList;
     FStruct:       TDisAsmStruct;
  protected
     // Protected declarations
     procedure      Clear;
     function       GetInstrCount: Integer;
     function       GetInstr(Index: Integer): TDisAsmInstr;
  public
     // Public declarations
     constructor    Create;
     destructor     Destroy; override;
     procedure      Disassemble(Address: Pointer; Size: Cardinal = 0);
     property       BaseAddress: Pointer read FAddress;
     property       Count: Integer read GetInstrCount;
     property       Instructions[Index: Integer]: TDisAsmInstr read GetInstr; default;
     property       Size: Cardinal read FSize;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TDisAsm Class
////////////////////////////////////////////////////////////////////////////////
procedure TDisAsm.Disassemble(Address: Pointer; Size: Cardinal = 0);
var  lpszAddress:   PChar;
     daiAsm:        PDisAsmInstr;
     dwSize:        Cardinal;
     instr:         TLibInstr;
begin

  // Clear current instructions
  Clear;

  // Set base address
  FAddress:=Address;

  // Set total size to parse. If Size is zero (default) then process until
  // a RET instruction is encountered
  if (Size = 0) then
     dwSize:=MaxInt
  else
     dwSize:=Size;

  // Set starting size (offset)
  FSize:=0;

  // Check address
  if Assigned(FAddress) then
  begin
     // Set address to start processing
     lpszAddress:=Address;
     // Loop until RET or Size is reached
     while (FSize < dwSize) and (DisassembleAddress(FStruct, lpszAddress, instr) > 0) do
     begin
        // Create a new instruction record
        daiAsm:=AllocMem(SizeOf(TDisAsmInstr));
        // Fill instruction
        daiAsm^.Code:=instr;
        daiAsm^.Address:=lpszAddress;
        daiAsm^.OffsetFromBase:=FSize;
        // Add the instruction to the list
        FInstrList.Add(daiAsm);
        // Increment the current address and offset
        Inc(lpszAddress, instr.size);
        Inc(FSize, instr.size);
        // If instruction is a RET and Size = zero then break
        if ((instr.mnemType = INS_RET) and (Size = 0)) then break;
     end;
  end;

end;

function TDisAsm.GetInstrCount: Integer;
begin

  // Return the count of instructions
  result:=FInstrList.Count;

end;

function TDisAsm.GetInstr(Index: Integer): TDisAsmInstr;
begin

  // Return the instruction at the specified index
  result:=PDisAsmInstr(FInstrList[Index])^;

end;

procedure TDisAsm.Clear;
var  daiAsm:        PDisAsmInstr;
     dwCount:       Integer;
begin

  // Release all the memory allocated to the instructions
  for dwCount:=Pred(FInstrList.Count) downto 0 do
  begin
     // Get the instruction
     daiAsm:=FInstrList[dwCount];
     // Free the memory
     FreeMem(daiAsm);
  end;

  // Clear the list
  FInstrList.Clear;

end;

constructor TDisAsm.Create;
begin

  // Perform inherited
  inherited Create;

  // Set starting defaults
  FSize:=0;
  FAddress:=nil;
  FInstrList:=TList.Create;

  // Initialize the disassembler struct
  DisassembleInit(FStruct, 0, INTEL_SYNTAX);

end;

destructor TDisAsm.Destroy;
begin

  // Clear all instruction codes from the list, releasing all allocated memory
  Clear;
  FInstrList.Free;

  // Cleanup the disassembler struct
  DisassembleCleanup(FStruct);

  // Perform inherited
  inherited Destroy;

end;

////////////////////////////////////////////////////////////////////////////////
// Low Level Disassembly functions
////////////////////////////////////////////////////////////////////////////////
function GetSignSizedOperand(Op: PInteger; Buffer: Pointer; Size: Integer): Integer;
begin

	// Copy size bytes from Buffer to Op and return number of bytes copied
	case Size of
     1  :  Op^:=ShortInt(Buffer^);
     2  :  Op^:=SmallInt(Buffer^);
     4  :  Op^:=Integer(Buffer^);
     6,
     8  :  Op^:=Int64(Buffer^);
  else
     Op^:=Integer(Buffer^);
  end;

  // Return the size
  result:=Size;

end;

function GetSizedOperand(var Op: Cardinal; Buffer: Pointer; Size: Integer): Integer;
begin

	// Copy size bytes from Buffer to Op and return number of bytes copied
	case Size of
     1  :  Op:=Byte(Buffer^);
     2  :  Op:=Word(Buffer^);
     4  :  Op:=Cardinal(Buffer^);
     6,
     8  :  Op:=Int64(Buffer^);
  else
     Op:=Cardinal(Buffer^);
  end;

  // Return the size
  result:=Size;

end;

procedure DecodeByte(b: Byte; modrm: PmodRMbyte);
begin

	// Generic bitfield-packing routine
	modrm^._mod:=b shr 6;
	modrm^.reg:=(b and 56) shr 3;
	modrm^.rm:=b and 7;

end;

function DecodeSIB(var Struct: TDisAsmStruct; b: PByteArray; _mod: Integer): Integer;
var  count:      Integer;
     sib:        SIB_byte;
begin

	// Set Address Expression fields (scale, index, base, disp)
	// according to the contents of the SIB byte.
	// b points to the SIB byte in the instruction-stream buffer; the
	// byte after b[0] is therefore the byte after the SIB
	// returns number of bytes 'used', including the SIB byte

  // Start at 1 for SIB byte
	count:=1;

  // Fill in the type with defaults. Delphi does not allow constant
  // value is types, thus the reason for the assignment
	sib:=cSIB_byte;

  // Decode the byte value
	DecodeByte(b^[0], @sib);

	if ((sib.base = SIB_BASE_EBP) and (_mod = 0)) then
  begin
     // if base == 101 (ebp)
     // IF BASE == EBP, deal with exception
		// IF (ModR/M did not create a Disp
		// ... create a 32-bit Displacement
		GetSignSizedOperand(@Struct.expr.disp, @b^[1], SizeOf(Integer));
		// Mark Addr Expression as having a DWORD for DISP
		Struct.expr.flags:=Struct.expr.flags or ADDREXP_DWORD shl ADDEXP_DISP_OFFSET;
		Inc(count, 4);
  end
  else
  begin
		// ELSE BASE refers to a General Register
		Struct.expr.base:=sib.base;
		// Mark Addr Expression as having a register for BASE
		Struct.expr.flags:=Struct.expr.flags or ADDREXP_REG shl ADDEXP_BASE_OFFSET;
	end;
	if (sib.scale > 0) then
  begin
		// IF SCALE is not '1'
		Struct.expr.scale:= $01 shl sib.scale;	// scale becomes 2, 4, 8
		// Mark Addr Expression as having a BYTE for SCALE
		Struct.expr.flags:=Struct.expr.flags or ADDREXP_BYTE shl ADDEXP_SCALE_OFFSET;
	end;
	if (sib.index <> SIB_INDEX_NONE) then
  begin
		// IF INDEX is not 'ESP' (100)
		Struct.expr.index:=sib.index;
		// Mark Addr Expression as having a register for INDEX
		Struct.expr.flags:=Struct.expr.flags or ADDREXP_REG shl ADDEXP_INDEX_OFFSET;
	end;
  result:=Count;

end;

function AddrExpNew(var Struct: TDisAsmStruct; Scale, Index, Base, Disp, Flags: Integer): Integer;
begin

  // Figure out which exp to use
  if (Struct.exp[0].used = 0) then
     result:=0
  else if (Struct.exp[1].used = 0) then
     result:=1
  else
     result:=2;

  // Set to used
  Struct.exp[result].used:=1;
  Struct.exp[result].scale:=Scale;
  Struct.exp[result].index:=Index;
  Struct.exp[result].base:=Base;
  Struct.exp[result].disp:=Disp;
  Struct.exp[result].flags:=Flags;

end;

function DecodeModRM(var Struct: TDisAsmStruct; b: PByteArray; var Op, op_flags: Cardinal; reg_type, size, flags: Integer): Integer;
var  modrm:      modRM_byte;
     disp_start: PChar;
     count:      Integer;
begin

	// Create address expression and/or fill operand based on value of
	// ModR/M byte. Calls DecodeSIB as appropriate.
	//    b points to the loc of the modR/M byte in the instruction stream
	//    op points to the operand buffer
	//    op_flags points to the operand flags buffer
	//    reg_type encodes the type of register used in this instruction
	//    size specifies the default operand size for this instruction
	//    flags specifies whether the Reg or mod+R/M fields are being decoded
	// returns the number of bytes in the instruction, including modR/M
  modrm:=cmodRM_byte;

  // Get bit fields
	DecodeByte(b^[0], @modrm);

  // Check flags
	if (flags = MODRM_EA) then
  begin
     // # of bytes decoded [1 for modR/M byte]
	   count:=1;
		// IF this is the mod + R/M operand
		if (modrm._mod = MODRM_MOD_NOEA) then
     begin
        // if mod == 11
			// IF MOD == Register Only, no Address Expression
			Op:=modrm.rm + reg_type;
			op_flags:=op_flags and $FFFFF0FF;
        // Flag operand as Register
        op_flags:=op_flags or OP_REG;
     end
		else if (modrm._mod = MODRM_MOD_NODISP) then
     begin
     	// if mod == 00
			// IF MOD == No displacement, just Indirect Register
			if (modrm.rm = MODRM_RM_NOREG) then
        begin
        	// if r/m == 101
				// IF RM == No Register, just Displacement
				// This is an Intel Moronic Exception TM
				if (Struct.mode16 <> 0) then
           begin
					// IF operand size is 16 bit
					GetSignSizedOperand(@Struct.expr.disp, @b^[1], SizeOf(Word));
					// Flag Addr Expr as having WORD for DISP
					Struct.expr.flags:=Struct.expr.flags or ADDREXP_WORD shl ADDEXP_DISP_OFFSET;
              // Add disp size to count
					Inc(count, 2);
           end
				else
           begin
					// ELSE Operand size is 32-bit
					GetSignSizedOperand(@Struct.expr.disp, @b^[1], SizeOf(Integer));
					// Flag Addr Expr as having DWORD for DISP
					Struct.expr.flags:=Struct.expr.flags or ADDREXP_DWORD shl ADDEXP_DISP_OFFSET;
              // Add disp size to count
					Inc(count, 4);
           end;
        end
			else if (modrm.rm = MODRM_RM_SIB) then
        begin
           // If r/m == 100 
				// ELSE IF an SIB byte is present
           Inc(count, DecodeSIB(Struct, @b^[1], modrm._mod));
        end
			else
        begin
           // modR/M specifies base register
				// ELSE RM encodes a general register
				Struct.expr.base:=modrm.rm;
				// Flag AddrExpression as having a REGISTER for BASE
           Struct.expr.flags:=Struct.expr.flags or ADDREXP_REG shl ADDEXP_BASE_OFFSET;
			end;
        // Flag operand as Address Expr
        op_flags:=op_flags and $FFFFF0FF;
        op_flags:=op_flags or OP_EXPR;
     end
     else
     begin
		   // mod is 01 or 10
			if (modrm.rm = MODRM_RM_SIB) then
        begin
           // rm == 100
				// IF base is an AddrExpr specified by an SIB byte
				disp_start:=@b^[2];
           Inc(count, DecodeSIB(Struct, @b^[1], modrm._mod));
        end
			else
        begin
				// ELSE base is a general register
				disp_start:=@b^[1];
           // Always a general_dword reg
				Struct.expr.base:=modrm.rm;
				// Flag AddrExpression as having a REGISTER for BASE
           Struct.expr.flags:=Struct.expr.flags or ADDREXP_REG shl ADDEXP_BASE_OFFSET;
        end;
			// ELSE mod + r/m specify a disp##[base] or disp##(SIB)
			if (modrm._mod = MODRM_MOD_DISP8) then
        begin
           // mod == 01
				// If this is an 8-bit displacement
				// uh, this needs to be sign-extended
				GetSignSizedOperand(@Struct.expr.disp, disp_start, SizeOf(Char));
				// Flag AddrExpression as having a BYTE for DISP
				Struct.expr.flags:=Struct.expr.flags or ADDREXP_BYTE shl ADDEXP_DISP_OFFSET;
           Inc(count);
        end
        else
        begin
				// Displacement is dependent on address size
				if (Struct.mode16 <> 0)  then
           begin
					GetSignSizedOperand(@Struct.expr.disp, disp_start, SizeOf(Word));
					// Flag AddrExpression as having WORD for DISP
					Struct.expr.flags:=Struct.expr.flags or ADDREXP_WORD shl ADDEXP_DISP_OFFSET;
              Inc(count, 2);
           end
           else
           begin
					GetSignSizedOperand(@Struct.expr.disp, disp_start, SizeOf(Integer));
					// Flag AddrExpression as having DWORD for DISP
					Struct.expr.flags:=Struct.expr.flags or ADDREXP_DWORD shl ADDEXP_DISP_OFFSET;
              Inc(count, 4);
           end;
        end;
        // Flag operand as Address Expr
        op_flags:=op_flags and $FFFFF0FF;
        op_flags:=op_flags or OP_EXPR;
     end;
		if (Struct.expr.flags <> 0) then
     begin
			// IF address expression was created for this instruction
			// Set Operand to the ID of the AddrExpr
        Op:=AddrExpNew(Struct, Struct.expr.scale, Struct.expr.index, Struct.expr.base, Struct.expr.disp, Struct.expr.flags);
		end;
  end
  else
  begin
		// ELSE this is the 'reg' field : assign a register
		// set operand to register ID
     Op:=modrm.reg + reg_type;
     op_flags:=op_flags or OP_REG;
		count:=0;
	end;

  // Return number of bytes for instruction
  result:=count;

end;

procedure ApplySeg(prefix: Cardinal; var dest_flg, dest: Cardinal; expr: Paddr_exp);
var  seg:        Cardinal;
     _type:      Cardinal;
begin

	seg:=prefix and $F0000000;

	if (seg > 0) then
  begin
	   // Apply defaults for each register
	   _type:=OP_DATASEG;
     {
	   if ((dest_flg and OP_TYPE_MASK) = OP_EXPR) then
		   reg:=expr^.base
	   else if ((dest_flg and OP_TYPE_MASK) = OP_REG) then
		   reg:=dest;
     }
	   // reg is either 0 {eax) or from operand
	   // Apply overrides from prefix
     case seg of
	      PREFIX_CS   :  _type:=OP_CODESEG;
	      PREFIX_SS   :  _type:=OP_STACKSEG;
	      PREFIX_DS   :  _type:=OP_DATASEG;
	      PREFIX_ES   :  _type:=OP_EXTRASEG;
	      PREFIX_FS   :  _type:=OP_DATA1SEG;
	      PREFIX_GS   :  _type:=OP_DATA2SEG;
     end;
     dest_flg:=dest_flg or _type;
	end;

end;

function InstDecode(var Struct: TDisAsmStruct; t: Pinstr; buf: PByteArray; c: Pcode; rva: DWORD): Integer;
var  x:          Integer;
     bytes:      Integer;
     size:       Integer;
     op_size_flg:Cardinal;
     addr_size:  Cardinal;
     op_size:    Cardinal;
     op_notes:   Cardinal;
     addr_meth:  Cardinal;
     op_type:    Cardinal;
     prefix:     Cardinal;
     genRegs:    Integer;
     operands:   Array [0..2] of Integer;
     op_flags:   Array [0..2] of Integer;
     dest_buf:   TExpArray;
     dest_flg:   TExpArray;
begin

	// Decode the operands of an instruction; calls DecodeModRM as
  // necessary, gets displacemnets and immeidate values, and sets the
	// values of operand and operand flag fields in the code struct.
	// buf points to the byte *after* the opcode of the current instruction
	// in the instruction stream
	//    t points to the representation of the instruction in the opcode table
	//    c points to the destination code structure which we are in the
  //       process of filling
	//    rva is the virtual address of the start of the current instruction;
	//       it may or may not prove useful.
	//    returns number of bytes found in addition to the actual opcode bytes
	// note: bytes defaults to 0, since disasm_addr takes care of the
  // opcode size ... everything else is dependent on operand types

	// bytes: size of curr instr; size: operand size
	bytes:=0;
  size:=0;
  op_size_flg:=0;

	// Tables used to address each operands with the for loop
	operands[0]:=t^.dest;
  operands[1]:=t^.src;
  operands[2]:=t^.aux;
	op_flags[0]:=t^.destFlg;
	op_flags[1]:=t^.srcFlg;
	op_flags[2]:=t^.auxFlg;
  dest_buf[0]:=@c^.dest;
  dest_buf[1]:=@c^.src;
  dest_buf[2]:=@c^.aux;
  dest_flg[0]:=@c^.destType;
  dest_flg[1]:=@c^.srcType;
  dest_flg[2]:=@c^.auxType;

	// Clear global ADDRESS EXPRESSION struct
  ZeroMemory(@Struct.expr, SizeOf(addr_exp));

	// Set addressing mode
  Struct.mode16:=Struct.settings.options and LEGACY_MODE;

	//  ++++   1. Copy mnemonic and mnemonic-flags to CODE struct
	if (t^.mnemonic[0] > #0) then
		// IF the instruction has a mnemonic, cat it to the mnemonic field
		StrLCat(c^.mnemonic, t^.mnemonic, 15);

  // Save INS_TYPE flags
  c^.mnemType:=c^.mnemType or t^.mnemFlg;

	//  ++++   2. Handle opcode prefixes
	prefix:=c^.mnemType and $FFF00000;
	c^.mnemType:=c^.mnemType and $000FFFFF;
  // Set Address Size to Default Addr Size
	addr_size:=Struct.settings.sz_addr;
	if ((prefix and PREFIX_ADDR_SIZE) <> 0) then
  begin
		// IF Address Size Override Prefix is set
		if (addr_size = 4) then
     begin
			addr_size:=2;	// that's right, it's a toggle
			Struct.mode16:=1;
     end
		else
     begin
        addr_size:=4;
			Struct.mode16:=0;
     end;
  end;

  // Set Operand Size to Default Operand Size
	op_size:=Struct.settings.sz_oper;
	if ((Prefix and PREFIX_OP_SIZE) <> 0) then
  begin
		// IF Operand Size Override Prefix is set
		if (op_size = 4) then
			op_size:=2
		else
			op_size:=4;
	end;

	// These prepend the relevant string to the mnem
	if ((prefix and PREFIX_LOCK) <> 0) then
		c^.mnemType:=c^.mnemType or INS_LOCK;
	if ((prefix and PREFIX_REPNZ) <> 0) then
		c^.mnemType:=c^.mnemType or INS_REPNZ;
	if ((prefix and PREFIX_REP) <> 0) or ((prefix and PREFIX_REPZ) <> 0) then
		c^.mnemType:=c^.mnemType or INS_REPZ;
	// this is ignored :P 
	// if (prefix & PREFIX_SIMD) ;

	//  ++++   3. Fill operands and operand-flags in CODE struct
	for x:=0 to 2 do
  begin
		// FOREACH Operand in (dest, src, aux)
		// set default register set to 16- or 32-bit regs
		// ++ Yank optype and addr mode out of operand flags
		addr_meth:=op_flags[x] and ADDRMETH_MASK;
		op_type:=op_flags[x] and OPTYPE_MASK;
		op_notes:=op_flags[x] and OPFLAGS_MASK;
		// Clear flags for this operand
		dest_flg[x]^:=0;
		// ++ Copy flags from opcode table to CODE struct
		dest_flg[x]^:=dest_flg[x]^ or op_notes;
		// ++ Handle operands hard-coded in the opcode [e.g. "dec eax"]
		if ((operands[x] <> 0) or ((op_flags[x] and OP_REG) <> 0)) then
     begin
			// operands[x] contains either an Immediate Value or a Register ID
			dest_buf[x]^:=operands[x];
        // Next operand
			Continue;
     end;
		// ++ Do Operand Type ++
		case op_type of
			// This sets the operand Size based on the Intel Opcode Map
			// (Vol 2, Appendix A). Letter encodings are from section
			//  A.1.2, 'Codes for Operand Type'
			// NOTE: the bastard size encodings (OP_WORD etc) are based
			// on the size of the machine word, NOT Intel's f'ed-up
			// conception of a WORD (which is a machine-half word, or
        // 16 bits on a 32-bit platform). That will make the
        // following a bit confusing as it changes from Intel (or
			// "wrong") to bastard (or "right") size terminology.
		   OPTYPE_c       :
        begin
           // byte or word [op size attr]
           if (op_size = 4) then
              size:=2
           else
              size:=1;
           if (op_size = 4) then
              op_size_flg:=OP_HWORD
           else
              op_size_flg:=OP_BYTE;
        end;
		   OPTYPE_a       :
        begin
           // 2 word or 2 DWORD [op size attr ]
			   // when is this used?
           if (op_size = 4) then
              size:=4
           else
              size:=2;
           if (op_size = 4) then
              op_size_flg:=OP_WORD
           else
              op_size_flg:=OP_HWORD;
        end;
		   OPTYPE_v       :
        begin
           // word or dword [op size attr]
           if (op_size = 4) then
              size:=4
           else
              size:=2;
           if (op_size = 4) then
              op_size_flg:=OP_WORD
           else
              op_size_flg:=OP_HWORD;
        end;
		   OPTYPE_p       :
        begin
           // 32/48-bit ptr [op size attr]
           if (op_size = 4) then
              size:=6
           else
              size:=4;
           if (op_size = 4) then
              op_size_flg:=OP_DWORD
           else
              op_size_flg:=OP_WORD;
        end;
		   OPTYPE_b       :
        begin
           // byte, ignore op-size
			   size:=1;
			   op_size_flg:=OP_BYTE;
        end;
		   OPTYPE_w       :
        begin
           // word, ignore op-size
			   size:=2;
			   op_size_flg:=OP_HWORD;
        end;
		   OPTYPE_d       :
        begin
           // dword, ignore op-size
			   size:=4;
			   op_size_flg:=OP_WORD;
        end;
		   OPTYPE_s       :
        begin
           // 6-byte psuedo-descriptor
			   size:=6;
			   op_size_flg:=OP_DWORD;
        end;
		   OPTYPE_q       :
        begin
           // qword, ignore op-size
			   size:=8;
			   op_size_flg:=OP_DWORD;
        end;
		   OPTYPE_dq,
		   OPTYPE_ps,
		   OPTYPE_ss      :
        begin
           // Scalar elem of 128-bit FP data
			   size:=16;
			   op_size_flg:=OP_QWORD;
        end;
		   OPTYPE_pi,
        OPTYPE_si      :  ;	// qword mmx register or dword integer register
		   OPTYPE_fs      :
        begin
           // single-real
			   size:=4;
			   op_size_flg:=OP_SREAL;
			end;
		   OPTYPE_fd      :
        begin
           // double real
			   size:=4;
			   op_size_flg:=OP_DREAL;
			end;
		   OPTYPE_fe      :
        begin
           // extended real
			   size:=4;
			   op_size_flg:=OP_XREAL;
			end;
		   OPTYPE_fb      :
        begin
           // packed BCD
			   size:=4;
			   op_size_flg:=OP_BCD;
			end;
		   OPTYPE_fv      :
        begin
        	// FPU env: 14/28-bytes
			   size:=4;
			   op_size_flg:=OP_FPENV;
			end;
		   OPTYPE_m       :
        begin
           // fake operand type used for "lea Gv, M"
			   size:=addr_size;
           if (addr_size = 4) then
              op_size_flg:=OP_DWORD
           else
              op_size_flg:=OP_WORD;
			end;
     end;

		// Override default register set based on size of Operand Type
		// this allows mixing of 8, 16, and 32 bit regs in instruction
		if (size = 1) then
			genRegs:=REG_BYTE_OFFSET
		else if (size = 2) then
			genRegs:=REG_WORD_OFFSET
		else
			genRegs:=REG_DWORD_OFFSET;

		// ++ Do Operand Addressing Method / Decode operand ++
		case addr_meth of
		   // This sets the operand Size based on the Intel Opcode Map
		   // (Vol 2, Appendix A). Letter encodings are from section
		   // A.1.1, 'Codes for Addressing Method'

		   // ---------------------- Addressing Method --------------
		   // Note that decoding mod ModR/M operand adjusts the size of
		   // the instruction, but decoding the reg operand does not.
		   // This should not cause any problems, as every 'reg' operand
		   // has an associated 'mod' operand.
		   //   dest_flg[x] points to a buffer for the flags of current operand
		   //   dest_buf[x] points to a buffer for the value of current operand
		   //   bytes is a running total of the instruction size
		   // Goddamn-Intel-Note:
		   //   Some Intel addressing methods [M, R] specify that the modR/M
		   //   byte may only refer to a memory address or may only refer to
		   //   a register -- however Intel provides no clues on what to do
		   //   if, say, the modR/M for an M opcode decods to a register
		   //   rather than a memory address ... retuning 0 is out of the
		   //   question, as this would be an Immediate or a RelOffset, so
		   //   instead these modR/Ms are decoded according to opcode table.
		   ADDRMETH_E     :
        begin
           // ModR/M present, Gen reg or memory
			   Inc(bytes, DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, genRegs, addr_size, MODRM_EA));
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
			   ApplySeg(prefix, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
			end;
		   ADDRMETH_M     :
        begin
           // ModR/M only refers to memory
			   Inc(bytes, DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, genRegs, addr_size, MODRM_EA));
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
           ApplySeg(prefix, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
        end;
		   ADDRMETH_Q     :
        begin
           // ModR/M present, MMX or Memory
			   Inc(bytes, DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, REG_MMX_OFFSET, addr_size, MODRM_EA));
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
           ApplySeg(prefix, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
			end;
		   ADDRMETH_R     :
        begin
           // ModR/M mod == gen reg
			   Inc(bytes, DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, genRegs, addr_size, MODRM_EA));
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
           ApplySeg(prefix, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
			end;
		   ADDRMETH_W     :
        begin
           // ModR/M present, mem or SIMD reg
			   Inc(bytes, DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, REG_SIMD_OFFSET, addr_size, MODRM_EA));
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
           ApplySeg(prefix, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
        end;
			// MODRM -- reg operand
			// TODO: replace OP_REG with register type flags
		   ADDRMETH_C     :
        begin
           // ModR/M reg == control reg
			   DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, REG_CTRL_OFFSET, size, MODRM_reg);
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
        end;
		   ADDRMETH_D     :
        begin
           // ModR/M reg == debug reg
			   DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, REG_DEBUG_OFFSET, size, MODRM_reg);
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
        end;
		   ADDRMETH_G     :
        begin
        	// ModR/M reg == gen-purpose reg
			   DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, genRegs, size, MODRM_reg);
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
        end;
		   ADDRMETH_P     :
        begin
        	// ModR/M reg == qword MMX reg
			   DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, REG_MMX_OFFSET, size, MODRM_reg);
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
        end;
		   ADDRMETH_S     :
        begin
           // ModR/M reg == segment reg
			   DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, REG_SEG_OFFSET, size, MODRM_reg);
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
        end;
		   ADDRMETH_T     :
        begin
           // ModR/M reg == test reg
			   DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, REG_TEST_OFFSET, size, MODRM_reg);
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
        end;
		   ADDRMETH_V     :
        begin
           // ModR/M reg == SIMD reg
			   DecodeModRM(Struct, buf, dest_buf[x]^, dest_flg[x]^, REG_SIMD_OFFSET, size, MODRM_reg);
			   dest_flg[x]^:=dest_flg[x]^ or op_size_flg;
        end;
			// No MODRM
		   ADDRMETH_A     :
        begin
           // No modR/M -- direct addr
           dest_flg[x]^:=dest_flg[x]^ or OP_ADDR or op_size_flg;
			   GetSizedOperand(dest_buf[x]^, @buf^[bytes], addr_size);
           ApplySeg(prefix, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
           Inc(bytes, size);
        end;
		   ADDRMETH_F     :
        begin
           // EFLAGS register
           dest_flg[x]^:=dest_flg[x]^ or OP_REG or op_size_flg;
           dest_buf[x]^:=REG_FLAGS_INDEX;
        end;
		   ADDRMETH_I     :
        begin
           // Immediate val will this cause probs when doing dest=IMM, src=modR/M ?
           dest_flg[x]^:=dest_flg[x]^ or OP_IMM or op_size_flg;
           if ((dest_flg[x]^ and OP_SIGNED) <> 0) then
              GetSignSizedOperand(PInteger(dest_buf[x]), @buf[bytes], size)
           else
              GetSizedOperand(dest_buf[x]^, @buf[bytes], size);
			   Inc(bytes, size);
        end;
		   ADDRMETH_J     :
        begin
           // Rel offset to add to IP [jmp]
           dest_flg[x]^:=dest_flg[x]^ or OP_REL or OP_SIGNED or op_size_flg;
			   GetSizedOperand(dest_buf[x]^, @buf[bytes], size);
           Inc(bytes, size);
        end;
        ADDRMETH_O     :
        begin
           // No ModR/M;operand is word/dword offset
			   // NOTE: these are actually RVA's and not offsets to IP!!!
           dest_flg[x]^:=dest_flg[x]^ or OP_OFF or OP_SIGNED or op_size_flg;
			   GetSizedOperand(dest_buf[x]^, @buf[bytes], addr_size);
           ApplySeg(prefix, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
           Inc(bytes, size);
        end;
		   ADDRMETH_X     :
        begin
           // Memory addressed by DS:SI [string!]
           dest_flg[x]^:=dest_flg[x]^ or OP_STRING or OP_REG or op_size_flg;
			   // Set Operand to ID for register ESI
			   dest_buf[x]^:=6 + REG_DWORD_OFFSET;
           ApplySeg(PREFIX_DS, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
        end;
		   ADDRMETH_Y     :
        begin
        	// Memory addressed by ES:DI [string ]
           dest_flg[x]^:=dest_flg[x]^ or OP_STRING or OP_REG or op_size_flg;
			   // Set Operand to ID for register EDI
			   dest_buf[x]^:=7 + REG_DWORD_OFFSET;
           ApplySeg(PREFIX_ES, dest_flg[x]^, dest_buf[x]^, @Struct.expr);
        end;
     else
        // Operand is not used
			dest_flg[x]^:=0;
     end;
  end;

  // Return number of bytes in instructon
	result:=bytes;

end;

function GetRegName(var Struct: TDisAsmStruct; Index: Integer): PChar;
begin

  // Return the register name
  if (Index >= Struct.settings.sz_regtable) then
     result:=#0
  else
     result:=Struct.settings.reg_table[index].mnemonic;

end;

function FmtExprOp(var Struct: TDisAsmStruct; Operand, Flags: Integer; Buffer: PChar; Len: Cardinal): Integer;
begin

  if ((Operand = 0) and (Flags <> ADDREXP_REG)) then
     Buffer[0]:=#0
  else
  begin
     case Flags of
        ADDREXP_REG    :  StrLCopy(Buffer, GetRegName(Struct, operand), Len);
        ADDREXP_WORD   :  if (Operand <> 0) then StrLFmt(Buffer, Len, '0x%.4x', [Word(Operand)]);
        ADDREXP_DWORD  :  if (Operand <> 0) then StrLFmt(Buffer, Len, '0x%.8x', [Operand]);
        ADDREXP_QWORD  :  if (Operand <> 0) then StrLFmt(Buffer, Len, '0x%.12x', [Operand]);
     else
       if (Operand <> 0) then StrLFmt(Buffer, Len, '0x%.2x', [Byte(Operand)]);
     end;
  end;
  result:=StrLen(Buffer);

end;

function SprintAddrExp(var Struct: TDisAsmStruct; Str: PChar; Len: Cardinal; e: Paddr_exp): Integer;
var  scale:      Array [0..31] of Char;
     index:      Array [0..31] of Char;
     base:       Array [0..31] of Char;
     disp:       Array [0..31] of Char;
     tmp:        Array [0..31] of Char;
     idx:        Array [0..15] of Char;
     sd:         Char;
begin

  ZeroMemory(@scale, SizeOf(scale));
  ZeroMemory(@index, SizeOf(index));
  ZeroMemory(@base, SizeOf(base));
  ZeroMemory(@disp, SizeOf(disp));
  ZeroMemory(@tmp, SizeOf(tmp));
  ZeroMemory(@idx, SizeOf(idx));

  // Normalize negatives
  if (e^.disp < 0) then
  begin
     sd:='-';
    e^.disp:=e^.disp * -1;
  end
  else
     sd:='+';

  // Do scale
  FmtExprOp(Struct, e^.scale, AddrExp_ScaleType(e^.flags), @scale, 32);
  // Do index
  FmtExprOp(Struct, e^.index, AddrExp_IndexType(e^.flags), @index, 32);
  // Do byte
  FmtExprOp(Struct, e^.base, AddrExp_BaseType(e^.flags), @base, 32);
  // Do disp
  FmtExprOp(Struct, e^.disp, AddrExp_DispType(e^.flags), @disp, 32);

  Str[0]:=#0;
  if (scale[0] <> #0) and (index[0] <> #0) then
     StrLFmt(@idx, 16, '(%s*%s)', [scale, index])
  else if (index[0] <> #0) then
     StrLFmt(@idx, 16, '%s', [index]);
  if (base[0] <> #0) then
  begin
     StrLFmt(Str, 32, '[%s', [base]);
     if (idx[0] <> #0) then
     begin
        StrLCat(Str, '+', Len - StrLen(Str));
        StrLCat(Str, @idx, Len - StrLen(Str));
     end;
     if (disp[0] <> #0) then
     begin
        StrLFmt(@tmp, 32, '%s%s', [sd, disp]);
        StrLCat(Str, @tmp, Len - StrLen(Str));
     end;
     StrLCat(Str, ']', Len - StrLen(Str));
  end
  else if (idx[0] <> #0) then
     StrLFmt(Str, Len, '%s%s%s', [idx, sd, disp])
  else
  begin
     if (sd = '-' ) then StrLCat(Str, '-', Len - Strlen(Str));
     StrLCat(Str, @disp, Len - StrLen(Str));
  end;
  result:=StrLen(Str);

end;

function SprintSeg(var Struct: TDisAsmStruct; Str: PChar; Len: Cardinal; Seg: Integer): Integer;
begin

  seg:=seg shr 16;
  StrLFmt(Str, Len, '%s:', [GetRegName(Struct, Struct.settings.reg_seg + Seg - 1)]);
  result:=StrLen(Str);

end;

function SprintOp(var Struct: TDisAsmStruct; Str: PChar; Len, Op, _Type: Integer): Integer;
var  diff:       Integer;
     seg:        Integer;
begin

  if (_Type = 0) then
     ZeroMemory(Str, Len)
  else
  begin
     Str^:=#0;
     // Segment override for operand
     seg:=_Type and OP_SEG_MASK;
     case (_Type and OP_TYPE_MASK) of
        OP_PTR,
        OP_ADDR        :  StrFmt(Str, '0x%8x', [Op]);
        OP_REG         :
        begin
           if (seg <> 0) then
           begin
              diff:=SprintSeg(Struct, Str, Len, Seg);
              Inc(Str, diff);
              Dec(Len, diff);
           end;
           StrLFmt(Str, Len, '%s', [GetRegName(Struct, Op)]);
       end;
       OP_EXPR         : SprintAddrExp(Struct, Str, Len, @Struct.exp[Op]);
       OP_REL          :
        begin
         if (Op < 0) then
           begin
              Op:=Op * -1;
              StrLCat(Str, '-', Len);
           end
           else
              StrLCat(Str, '+', Len);
           Inc(Str);
           Dec(Len);
           StrLFmt(Str, Len, '0x%p', [Ptr(Op)]);
        end;
       OP_OFF          :  StrLFmt(Str, Len, '0x%.8x', [op]);
     else
        if ((_Type and OP_SIGNED) = OP_SIGNED) then
        begin
           if (op < 0) then
           begin
              StrLCat(Str, '-', Len);
              Inc(Str);
              Dec(Len);
              Op:=Op * -1;
           end;
           StrLFmt(Str, Len, '0x%p', [Ptr(Op)]);
        end
        else
           StrLFmt(Str, Len, '0x%p', [Ptr(Cardinal(Op))]);
     end;
  end;
  result:=StrLen(Str);

end;

procedure VmAddRegTblEntry(var Struct: TDisAsmStruct; Index: Integer; Name: PChar; Size, _Type: Integer);
begin

  // Add an entry to the table
  if (Index < Struct.settings.sz_regtable) then
  begin
     Struct.settings.reg_table^[index].size:=Size;
     Struct.settings.reg_table^[index]._type:=_Type;
     StrLCopy(Struct.settings.reg_table^[index].mnemonic, Name, 8);
  end;

end;

procedure InitRegTable(var Struct: TDisAsmStruct);
var  x:          Integer;
begin

  // Allocate memory for the tables
  with Struct do
  begin
     settings.sz_regtable:=86;
     settings.reg_table:=AllocMem(SizeOf(REGTBL_ENTRY) * 86);
     settings.reg_storage:=AllocMem(12 * 70);
  end;

  // Fill base table
  for x:=0 to 7 do
  begin
     // Add register : index into RegTable  Mnemonic   Size  Type
     VmAddRegTblEntry(Struct, REG_DWORD_OFFSET + x, rg_dword[x], REG_DWORD_SIZE, reg_gen_type[x]);
     VmAddRegTblEntry(Struct, REG_WORD_OFFSET + x, rg_word[x], REG_WORD_SIZE, reg_gen_type[x]);
     VmAddRegTblEntry(Struct, REG_BYTE_OFFSET + x, rg_byte[x], REG_BYTE_SIZE, REG_GENERAL);
     VmAddRegTblEntry(Struct, REG_MMX_OFFSET + x, rg_mmx[x], REG_MMX_SIZE, REG_SIMD);
     VmAddRegTblEntry(Struct, REG_SIMD_OFFSET + x, rg_simd[x], REG_SIMD_SIZE, REG_SIMD);
     VmAddRegTblEntry(Struct, REG_DEBUG_OFFSET + x, rg_debug[x], REG_DEBUG_SIZE, REG_SYS);
     VmAddRegTblEntry(Struct, REG_CTRL_OFFSET + x, rg_control[x], REG_CTRL_SIZE, REG_SYS);
     VmAddRegTblEntry(Struct, REG_TEST_OFFSET + x, rg_test[x], REG_TEST_SIZE, REG_SYS);
     VmAddRegTblEntry(Struct, REG_SEG_OFFSET + x, rg_seg[x], REG_SEG_SIZE, REG_SEG);
     VmAddRegTblEntry(Struct, REG_FPU_OFFSET + x, rg_fpu[x], REG_FPU_SIZE, REG_FPU);
  end;

  // Add the irregular registers
  VmAddRegTblEntry(Struct, REG_FLAGS_INDEX, 'eflags', REG_FLAGS_SIZE, REG_CC);
  VmAddRegTblEntry(Struct, REG_FPCTRL_INDEX, 'fpctrl', REG_FPCTRL_SIZE, REG_FPU or REG_SYS);
  VmAddRegTblEntry(Struct, REG_FPSTATUS_INDEX, 'fpstat', REG_FPSTATUS_SIZE, REG_FPU or REG_SYS);
  VmAddRegTblEntry(Struct, REG_FPTAG_INDEX, 'fptag', REG_FPTAG_SIZE, REG_FPU or REG_SYS);
  VmAddRegTblEntry(Struct, REG_EIP_INDEX, 'eip', REG_EIP_SIZE, REG_PC);
  VmAddRegTblEntry(Struct, REG_IP_INDEX, 'ip', REG_IP_SIZE, REG_PC);

end;

function DisAsmAddr(var Struct: TDisAsmStruct; buf: PByteArray; tbl: Integer; c: Pcode; rva: Integer): Integer;
var  op:         Byte;
     t:          PInstrArray;
     size:       Integer;
     x:          Integer;
begin

	// This instruction looks up index buf[0] in table tbl
	// (tables are in i386.opcode.map-> loaded from DisAsm.res). Note that some
  // tables require the index be masked or div'ed in order to convert a high-number
  // opcode [e.g. 85] to a reasonable low-number index (e.g. 2); also,
  // some tables mask only portions of the opcode (e.g., when an opcode
  // extension is in the reg field of the modR/M byte). This function
	// recurses, increasing the size of the intruction by >= 1 byte, when
	// an opcode contains additional bytes which reference another table
	// (e.g. 0x0F opcodes, opccodes extended to modR/M.reg, etc).
	//    buf points to the loc of the current opcode (start of the
	//       intruction) in the instruction stream. The instruction stream
	//       is assumed to be a buffer of bytes read directly from the file/memory
	//       for the purpose of disassembly; a mmapped file is ideal for this.
	//    tbl indicates which table to lookup this opcode in; 0 is the main
	//       opcode table and is the only table that should be referenced by
	//       a non-recursive call.
	//    c points to a code structure which will be filled by InstDecode
  //       rva is the virtual address of the current instruction
	//    returns the size of the decoded instruction in bytes

  // byte value -- 'opcode'
	op:=buf^[0];

  // Automatically advance position 1 byte
	size:=1;

  // Only do this for the main opcode table
	if ((tbl = 0) and ((Struct.settings.options and IGNORE_NULLS) <> 0) and (Integer(Pointer(buf)^) = 0)) then
		// IF this is the main opcode table AND
     // IGNORE_NULLS is set AND
     // the first 4 bytes in the intruction stream are NULL
     // THEN return 0 (END_OF_DISASSEMBLY)
     size:=0
  else
  begin
     if ((tables86[tbl].maxlim < $FF) and (op > tables86[tbl].maxlim)) then
     begin
		   // This is one of the f'ing FPU tables out of the 00-BH range
		   // did I mention how much I hate Intel? F'ing wankers...
        // My Note: I kept the comment above in respect of the original author;
        // right, wrong or otherwise... I have a sense of humor too ;-)
        Inc(tbl);
     end;
	   if (tables86[tbl].minlim <> 0) then
     begin
		   // This is one of the ultra-lame FPU tables [range C0-FF]
        Dec(op, tables86[tbl].minlim);
     end;
     // Get table from Master Table Table
     t:=tables86[tbl].table;
	   // Shift opcode extension into lowest bits [convert to table index]
	   op:=op shr tables86[tbl].shift;
     // This byte will be added to size by DecocdeModRM; therefore
	   // decrease the size here so the instr isn't 1 byte too large
     // Note: FPU w/ 0xFF mask use whole modR/M
     if (tables86[tbl].mask <> $FF) then Dec(size);
     // Mask out bits for opcode [0xFF] or modr/m opcode extension [0x07]
     op:=op and tables86[tbl].mask;
     // Reset return value to 0
	   x:=0;
	   // Use index 'op' into table 't' to make further decisions
	   if ((t^[op].mnemFlg and INSTR_PREFIX) <> 0) then
     begin
		   // IF this opcode is an Instruction Prefix */
        while (prefix_table[x, 0] <> 0) do
        begin
			   // Cheat by storing flag for prefix in the CODE struct.
			   // this enables it to be passed to instr decode w/o using
			   // any global vars or ruining the recursion of this function
			   if (prefix_table[x, 0] = op) then
           begin
				   // Strip multiple seg override prefixes
				   if ((c^.mnemType and PREFIX_REG_MASK) <> 0) and ((prefix_table[x, 1] and PREFIX_REG_MASK) <> 0) then
                 c^.mnemType:=c^.mnemType and $0FFFFFFF;
				   c^.mnemType:=c^.mnemType or Cardinal(prefix_table[x, 1]);
           end;
           Inc(x);
        end;
		   if (t^[op].mnemonic[0] > #0) then
			   // Some prefixs have no mnemonics
			   // IF prefix has a mnemonic, cat it to the instruction
			   StrLCat(c^.mnemonic, t^[op].mnemonic, 15);
		   // Recurse, Setting Opcode to next byte in instruction stream
        x:=DisAsmAddr(Struct, @buf^[1], tbl, c, rva);
     end
	   else if ((t^[op].table <> 0) and (t^[op].mnemonic[0] = #0)) then
     begin
        // ELSE if this opcode is an escape to another opcode table
		   // Recurse into new table, Setting Opcode to next byte in stream
        x:=DisAsmAddr(Struct, @buf^[1], t[op].table, c, rva);
     end
	   else if (t^[op].mnemonic[0] = #0) then
     begin
		   // ELSE if this is an invalid opcode, return zero bytes
		   x:=0;
		   StrCopy(c^.mnemonic, 'invalid');
     end
     else
     begin
		   // ELSE this is a valid opcode: disassemble it
		   x:=1+InstDecode(Struct, @t^[op], @buf[size], c, rva);
        Dec(size); // quick kludge so the if(x) turns out ok :)
     end;
     if (x = 0) then
		   // IF an invalid opcode was found then keep returning 'invalid'
		   size:=0
     else
        // Return total size
		   Inc(size, x);
  end;

  // Return size of instruction in bytes
	result:=size;

end;

////////////////////////////////////////////////////////////////////////////////
//   Exposed functions
////////////////////////////////////////////////////////////////////////////////
function DisassembleCleanup(var Struct: TDisAsmStruct): Integer;
begin

  // Release allocated memory
  if Assigned(Struct.settings.reg_table) then FreeMem(Struct.settings.reg_table);
  if Assigned(Struct.settings.reg_storage) then FreeMem(Struct.settings.reg_storage);

  // Clear the structure
  ZeroMemory(@Struct, SizeOf(TDisAsmStruct));

  // Return result
  result:=0;

end;

function DisassembleInit(var Struct: TDisAsmStruct; Options, Format: Integer): Integer;
begin

  // Clear the structure
  ZeroMemory(@Struct, SizeOf(TDisAsmStruct));

  // Set the options
  Struct.settings.options:=options;

  // Initialize the structure
  InitRegTable(Struct);

  // Set CPU specific information
  with Struct do
  begin
     settings.reg_seg:=REG_SEG_OFFSET;
     settings.reg_fp:=REG_FPU_OFFSET;
     settings.reg_in:=0;
     settings.reg_out:=0;
     if ((settings.options and LEGACY_MODE) = LEGACY_MODE) then
     begin
        settings.sz_addr:=2;
        settings.sz_oper:=2;
        settings.SP:=4 + REG_WORD_OFFSET;
        settings.FP:=5 + REG_WORD_OFFSET;
        settings.IP:=REG_IP_INDEX;
        settings.reg_gen:=REG_WORD_OFFSET;
     end
     else
     begin
        settings.sz_addr:=4;
        settings.sz_oper:=4;
        settings.SP:=4 + REG_DWORD_OFFSET;
        settings.FP:=5 + REG_DWORD_OFFSET;
        settings.IP:=REG_EIP_INDEX;
        settings.reg_gen:=REG_DWORD_OFFSET;
     end;
     settings.sz_inst:=MAX_INSTRUCTION_SIZE;
     settings.sz_byte:=8;
     settings.sz_word:=4;
     settings.sz_dword:=8;
     settings.endian:=LITTLE_ENDIAN_ORD;
  end;

  // Set result
  result:=ERROR_SUCCESS;

end;

function DisassembleAddress(var Struct: TDisAsmStruct; Buffer: Pointer; var Instruction: libinstr): Integer;
var  c:          code;
     lpszStr:    PChar;
     dest:       Array [0..31] of Char;
     src:        Array [0..31] of Char;
     aux:        Array [0..31] of Char;
begin

  // Clear the code
  ZeroMemory(@c, SizeOf(code));

  // Clear all 3 addr_exp's
  ZeroMemory(@Struct.exp, SizeOf(addr_exp) * 3);

  // Zero the passed instruction
  ZeroMemory(@Instruction, SizeOf(libinstr));

  // Call the low level disassembler function
  result:=DisAsmAddr(Struct, Buffer, 0, @c, 0);

  // Copy the native code struct to a more general instr struct
  StrLCopy(Instruction.mnemonic, c.mnemonic, 16);

  // Convert the instructions to text
  SprintOp(Struct, @dest, 32, c.dest, c.destType);
  SprintOp(Struct, @src, 32, c.src, c.srcType);
  SprintOp(Struct, @aux, 32, c.aux, c.auxType);

  // Format to buffer
  lpszStr:=@Instruction.szText;
  StrLFmt(lpszStr, 72, '%-20s%s', [c.mnemonic, dest]);
  if (src[0] > #0) then StrLFmt(lpszStr, 72 - StrLen(lpszStr), '%s, %s', [lpszStr, src]);
  if (aux[0] > #0) then StrLFmt(lpszStr, 72 - StrLen(lpszStr), '%s, %s', [lpszStr, aux]);

  // Set the instruction fields from the code
  Instruction.mnemType:=c.mnemType;
  Instruction.destType:=c.destType;
  Instruction.destVal:=c.dest;
  Instruction.srcType:=c.srcType;
  Instruction.srcVal:=c.src;
  Instruction.auxType:=c.auxType;
  Instruction.auxVal:=c.aux;
  Instruction.size:=result;

end;

////////////////////////////////////////////////////////////////////////////////
//   Table Data loading
////////////////////////////////////////////////////////////////////////////////
procedure LoadTableData;
var  hTable:     HRSRC;
     hTableRes:  HGLOBAL;
     lpTable:    Pointer;
     dwSize:     Cardinal;
     dwTable:    Integer;
begin

  // Load all table data
  for dwTable:=0 to High(tblLoad) do
  begin
     // Attempt to find the resource
     hTable:=FindResource(hInstance, tblLoad[dwTable].N, RT_RCDATA);
     Assert(hTable <> 0, Format('FindResource on %s failed', [tblLoad[dwTable].N]));
     if (hTable <> 0) then
     begin
        // Get the resource size
        dwSize:=SizeofResource(hInstance, hTable);
        Assert(dwSize = tblLoad[dwTable].S, Format('%s resource size is %d, actual size is %d', [tblLoad[dwTable].N, dwSize, tblLoad[dwTable].S]));
        // Load the resource
        hTableRes:=LoadResource(hInstance, hTable);
        Assert(hTableRes <> 0, Format('LoadResource on %s failed', [tblLoad[dwTable].N]));
        if (hTableRes <> 0) then
        begin
           // Lock the resource
           lpTable:=LockResource(hTableRes);
           Assert(Assigned(lpTable), Format('LockResource on %s failed', [tblLoad[dwTable].N]));
           // Move the table data into the table
           if Assigned(lpTable) then MoveMemory(tblLoad[dwTable].P, lpTable, dwSize);
        end;
     end;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
//   Macro Conversions
////////////////////////////////////////////////////////////////////////////////
function AddrExp_ScaleType(x: Integer): Integer;
begin

  // Return scale type
  result:=(x and ADDEXP_SCALE_MASK);

end;

function AddrExp_IndexType(x: Integer): Integer;
begin

  // Return index type
  result:=(x and ADDEXP_INDEX_MASK) shr 8;

end;

function AddrExp_BaseType(x: Integer): Integer;
begin

  // Return base type
  result:=(x and ADDEXP_BASE_MASK) shr 16;

end;

function AddrExp_DispType(x: Integer): Integer;
begin

  // Return disp type
  result:=(x and ADDEXP_DISP_MASK) shr 24;

end;

function cpu_endian(ext_arch: EXT__ARCH): Byte;
begin

  // Return ext_arch field
  result:=ext_arch.endian;

end;

function cpu_addr_size(ext_arch: EXT__ARCH): Byte;
begin

  // Return ext_arch field
  result:=ext_arch.sz_addr;

end;

function cpu_op_size(ext_arch: EXT__ARCH): Byte;
begin

  // Return ext_arch field
  result:=ext_arch.sz_oper;

end;

function cpu_word_size(ext_arch: EXT__ARCH): Byte;
begin

  // Return ext_arch field
  result:=ext_arch.sz_word;

end;

function cpu_inst_size(ext_arch: EXT__ARCH): Byte;
begin

  // Return ext_arch field
  result:=ext_arch.sz_inst;

end;

function cpu_sp(ext_arch: EXT__ARCH): Integer;
begin

  // Return ext_arch field
  result:=ext_arch.SP;

end;

function cpu_fp(ext_arch: EXT__ARCH): Integer;
begin

  // Return ext_arch field
  result:=ext_arch.FP;

end;

function cpu_ip(ext_arch: EXT__ARCH): Integer;
begin

  // Return ext_arch field
  result:=ext_arch.IP;

end;

initialization

  // Load the table data
  LoadTableData;

end.
