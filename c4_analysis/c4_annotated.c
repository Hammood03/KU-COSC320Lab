// c4.c - C in four functions
// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more
// Written by Robert Swierczek
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long
char *p, *lp, // current position in source code
     *data;   // data/bss pointer
int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};
// opcodes
//Assembly instructions
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };
// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };
//Reads next token from src and updates position
void next()
{
  //Pointer for the position (start of a token)
  char *pp;
  while (tk = *p) {
    //Update to next position (char) while we have tokens to go through
    ++p;
    //If a new line was encountered
    if (tk == '\n') {
      if (src) {
        printf("%d: %.*s", line, p - lp, lp);
        //Last printed position in source code is equal to pointer p
        lp = p;
        while (le < e) {
          //Print emitted assembly instructions
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n"); 
        }
      }
      //Increment the line (go to the next line)
      ++line;
    }
    //# is a token for comments as all characters are skipped until a new line is encountered or the null terminator
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') { //This is for identifiers (variable name). Notice that it cannot start with a number
      pp = p - 1; //Starting position for the identifier
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_') //Allowed characters for the identifier (Alphanumerical or _)
        tk = tk * 147 + *p++; //Compute a hash value for the token and then go to the next position
      tk = (tk << 6) + (p - pp); //Also hashing to have a unique value for the identifier
      id = sym; //For the symbols
      while (id[Tk]) { //Check whether the identifier has the same hash as one of the symbols and if yes, return
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; } //Here we compare the bytes to check if they are equal and end the function if they are.
        //p-pp is the identifier since p is the current position and pp is the first
        id = id + Idsz; //Update id to continue searching through the symbols
      }
      //If there was no conflict between the identifier and one of the symbols
      //Now add the identifier and its hash to id to prevent naming conflicts with other variables (variables cannot have the same identifier)
      id[Name] = (int)pp;
      id[Hash] = tk;
      tk = id[Tk] = Id;
      return;
    }
    else if (tk >= '0' && tk <= '9') { //Here we handle numericals in decimal, hexadecimal and octal
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; } //No 'X' or 'x' means decimals and the value is calculated in base 10 by multiplying each digit with 10^n from n=0 to the nth digit
      else if (*p == 'x' || *p == 'X') { //The presence of 'X' or 'x' before the number literals indicates a hexadecimal
        //Calculate the value for hex in base 16 by multiplying each digit with 16^n from n=0 to the nth digit and dealt with letters which are above 9 (A to F or a to f)
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      //Octals
      //Calculate the value for oct in base 8 similarly to decimal (but not base 10)
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
      //The token is a number
      tk = Num;
      return;
    }
    //If two consecutive // are encountered, just update the position until a new line or null terminator is encountered (skip)
    else if (tk == '/') {
      if (*p == '/') {
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      //If one / is encountered then the token is division operator
      else {
        tk = Div;
        return;
      }
    } 
    //Strings
    else if (tk == '\'' || tk == '"') {
      pp = data;
      //Iterate until a null terminator is encountered
      while (*p != 0 && *p != tk) {
        //Escape characters
        if ((ival = *p++) == '\\') {
          //This only handles the new line escape character
          if ((ival = *p++) == 'n') ival = '\n';
        }
        //End of string and assign to the value
        if (tk == '"') *data++ = ival;
      }
      //Update position
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;
      return;
    }
    //If the token is two consecutive (token is = and the next position is also =) = then it is equality, otherwise it is assignment.
    //Always go to the next position in these else-ifs until &
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    //If the token is two consecutive + then it is increment, otherwise it is addition
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    //If the token is two consecutive - then it is decrement, otherwise it is subtraction
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    //Not equal operator
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    //If we have two consecutive < then it is left shift, otherwise it is less than
    //If it is < and then = then it is less or equal
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    //Same logic as the above but it is greater than, greater or equal, or right shift
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    //If we have two consecutive | then it is logical OR, otherwise it is bitwise OR
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    //Same logic as above but AND
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    //If the token is ^ then it is xor
    else if (tk == '^') { tk = Xor; return; }
    //If the token is % then it is modulus
    else if (tk == '%') { tk = Mod; return; }
    //If the token is * then it is multiplication
    else if (tk == '*') { tk = Mul; return; }
    //If the token is [ then it is bracket
    else if (tk == '[') { tk = Brak; return; }
    //If the token is ? then it is the ternary condition operator
    else if (tk == '?') { tk = Cond; return; }
    //Immidiately return when one of these literals is encountered. Punctuation
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}
//Handling expressions
void expr(int lev)
{
  int t, *d;
//Terminate when there is an unexpected end of file
  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
  //If the token is a number add IMM and the value to the emitted code list (stack), go to the next token and set the type to integer
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  //If the the token is a string push IMM and the value to the stack and go to the next token
  else if (tk == '"') {
    *++e = IMM; *++e = ival; next();
    //Go to the next token as long as "" is encounterd
    while (tk == '"') next();
    //Align the data to correct size and set the type to pointer
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
  }
  //If the token is sizeof
  else if (tk == Sizeof) {
    //Check the paranthesis in the next position, if no paranthesis, terminate
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    //Set type and the default is int
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    //If the token is the Mul (Multiplication or Dereference in this case) operator, set the type to pointer along with its datatype (int or char)
    while (tk == Mul) { next(); ty = ty + PTR; }
    //Check the closing paranthesis in the next position, if no paranthesis, terminate
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    //Call sizeof and push its return value and set the default type (int)
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
    ty = INT;
  }
  //Identifiers
  else if (tk == Id) {
    //Call next
    d = id; next();
    //An identifier followed by ( is a function call
    if (tk == '(') {
      //Call next
      next();
      //Keep track of the number of arguments
      t = 0;
      //Parse and push arguments to the stack with "," as separator between arguments and increment t
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
      //Next position
      next();
      //System function
      //Push the value to the stack
      if (d[Class] == Sys) *++e = d[Val];
      //If not a system function, push JSR to the stack
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      //Otherwise terminate since it is a bad function call
      else { printf("%d: bad function call\n", line); exit(-1); }
      //Adjust the stack for arguments
      if (t) { *++e = ADJ; *++e = t; }
      //Set the return type 
      ty = d[Type];
    }
    //Identifier is bound to a number, a contsant
    //Push IMM and the value, and set the type to int
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    //Identifier is bound to reference
    //Load Effective Address (LEA) of the local reference and push the value
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      //If not local, then the identifier is bound to global variable
      //Push IMM and the value
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
      //If not any of the above, then terminate
      else { printf("%d: undefined variable\n", line); exit(-1); }
      //Push the local or global variable value to the stack
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  //If the token is ( then it is casting or expression
  else if (tk == '(') {
    //Call next
    next();
    //If the token is an int or char type then it is casting
    if (tk == Int || tk == Char) {
      //Type of cast and call next
      t = (tk == Int) ? INT : CHAR; next();
      //Pointer casting
      while (tk == Mul) { next(); t = t + PTR; }
      //If no closing paranthesis, terminate
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      //Parse expression after casting
      expr(Inc);
      //Change type to the casted type
      ty = t;
    }
    else {
      //Expression (no call or cast)
      expr(Assign);
      //If no closing paranthesis, terminate
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  //If then token is * then it is the dereference operator
  else if (tk == Mul) {
    //Call next and parse the expression
    next(); expr(Inc);
    //Dereference the pointer, unless it is a bad dereference then terminate
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    //Push the value referenced by the pointer
    *++e = (ty == CHAR) ? LC : LI;
  }
  //Reference operator
  else if (tk == And) {
    //Call next and parse the referenced expression
    next(); expr(Inc);
    //Check for valid address
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    //Convert the type to pointer
    ty = ty + PTR;
  }
  //Logical NOT operator
  //If there is an equality, negate it. Parse the expression and set the type to int
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  //Bitwise NOT operator. Parse the expression and set the type to int
  //XOR with -1 to obtain bitwise NOT
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  //Addition expression
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  //Subtraction
  else if (tk == Sub) {
    next(); *++e = IMM;
    //Multiply the number with -1 and call next
    if (tk == Num) { *++e = -ival; next(); } 
    //Multiplication with -1
    else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    //Set the type to int
    ty = INT;
  }
  //Pre-increment or pre-decrement
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else { printf("%d: bad expression\n", line); exit(-1); }
  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    //Process tokens until an operand with lower precedence is reached
    t = ty;
    //Assignment operation
    if (tk == Assign) {
      next();
      //Make sure the left hand side is valid
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      //Parse the right hand side
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    //Conditional (ternary)
    else if (tk == Cond) {
      next();
      //Deal with the false
      *++e = BZ; d = ++e;
      //Parse the expression if true
      expr(Assign);
      //Check for ":", if not present, terminate
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      //Jump to else
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      //Parse the expression if false
      expr(Cond);
      //End of if-else ternanry operator
      *d = (int)(e + 1);
    }
    //Logical OR, deal with the branch if true (BNZ), parse the right hand side of the expression and set type to int
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); //End of OR
       ty = INT; }
    //Logical OR, deal with the branch if false (BZ), parse the right hand side of the expression and set type to int
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); //End of AND
       ty = INT; }
    //Bitwise OR
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    //Bitwise XOR
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    //Bitwise AND
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    //Equality
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    //Not Equal
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    //Less than
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    //Grater than
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    //Less or equal
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    //Greater or equal
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    //Left shift
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    //Right shit
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    //Addition
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      //If adding an int with a pointer, multiply it with the size of it
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      *++e = ADD;
    }
    //Subtraction
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      //Subtract pointers and divide by the size (get number of elements)
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    //Mulitplication expression
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    //Division expression
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    //Modulus expression
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    //Postincrement or post-decrement
    else if (tk == Inc || tk == Dec) {
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      //Check for correct type, if invalid, terminate
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      //Pointer arithmetic (increment by size of type)
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    //Bracket expression for accessing array element
    else if (tk == Brak) {
      //Call next, push the array pointer to the stack and evaluate expression
      next(); *++e = PSH; expr(Assign);
      //If no closing bracket, terminate
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      //Access arrays (pointers)
      //Calculate offset by pushing index, size of element and multiplying them to get the right address
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      //If not pointer, terminate
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      //Add offset to the address of the pointer (index 0)
      *++e = ADD;
      //Retrieve value at calculated address
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}
//Statements
void stmt()
{
  //Pointers for jumping which is needed for conditional statements
  int *a, *b;
  //If statement
  if (tk == If) {
    //Call next (to next position)
    next();
    //Check for paranthesis
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    //Evaluate expression in if
    expr(Assign);
    //Check for closing paranthesis
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    //False branch of the condition and its address
    *++e = BZ; b = ++e;
    //Call recursively for the block of the conditional statement
    stmt();
    //Else statement
    if (tk == Else) {
      //Jump to skip the else block and store the address to jump to
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      //Call next and call the function recursively for the block
      next();
      stmt();
    }
    //Jump target for if-else block
    *b = (int)(e + 1);
  }
  //While loop
  else if (tk == While) {
    next();
    //Start of loop
    //This pointer will be used to jump to the start of the loop after finishing with the block
    a = e + 1;
    //Check for paranthesis
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    //Evaluate expression in the while loop condition
    expr(Assign);
    //Check for closing paranthesis
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    //False branch of the condition
    *++e = BZ; b = ++e;
    //Call recursively to parse the loop block
    stmt();
    //Jump to the start of the loop after the expressions in the block
    *++e = JMP; *++e = (int)a;
    //Jump to go outside the loop
    *b = (int)(e + 1);
  }
  //Return statement
  else if (tk == Return) {
    next();
    //Evaluate expression until a semicolon is found
    if (tk != ';') expr(Assign);
    //Leave instruction for return call
    *++e = LEV;
    //Check for semicolon
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  //Everything withing braces is a statement and a block
  else if (tk == '{') {
    next();
    while (tk != '}') stmt();
    next();
  }
  //Semicolon statement (nothing)
  else if (tk == ';') {
    next();
  }
  //Default: Expression as a statement which must end with a semicolon
  else {
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}
//main function
int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers. pc: program counter (return address). sp: stack pointer that points to the top of the stack and grows with it.
  //bp: base pointer which is used to define function boundry in the stack and used to access local variables especially in 32-bit architecture.
  //a: accumalator for storing for the ALU. cycle: cycle counter.
  int i, *t; // temps
  //Parse CLI arguments
  --argc; ++argv;
  //Source mode
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  //Debug mode
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }
  //Open source file
  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }
  //Allocate memory (256KB) for symbols, text, data, and stack
  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }
  //Memory area
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);
  //Keywords
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main
  //Load source code to memory
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);
  // parse declarations
  line = 1;
  next();
  while (tk) {
    bt = INT; // basetype
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = CHAR; }
    //For Enum initializing
    else if (tk == Enum) {
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    //Global variables and functions
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; }
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      //Check for duplicates
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // function
        //User-defined function
        id[Class] = Fun;
        id[Val] = (int)(e + 1);
        next(); i = 0;
        while (tk != ')') {
          //Types of arguments (int, char, pointer)
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          //Save identifiers as local variables
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          //Separator for arguments
          if (tk == ',') next();
        }
        next();
        //Check for braces
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        //Start of local variables
        loc = ++i;
        next();
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          //Contune local variable definition until semicolon
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            //Check for identifier
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            //Check for duplicates
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            //Local variable type and index
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        //Function prologue
        *++e = ENT; *++e = i - loc;
        while (tk != '}') stmt();
        //Function epilogue
        *++e = LEV;
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      //Store global variables in the data section of memory
      else {
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }
  //Look for main function and if not found, terminate. main() address must exist.
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0;
  // setup stack
  //Grow the stack according to the pool size, move sp to the top and then move bp to sp location
  bp = sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp; //Push old sp to the stack
  *--sp = argc; //Push argc
  *--sp = (int)argv; //Push argv
  *--sp = (int)t; //Push old sp address
  // run...
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle; //Update pc to point to the next instruction and cycle counter 
    if (debug) {
      //Debug mode operations
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(int *)a;                                     // load int
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    else if (i == PSH) *--sp = a;                                         // push
    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;
    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == FREE) free((void *)*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
