int pushOnRuntimeStack(int32_t value);
int popFromRuntimeStack(int32_t* node);
int popHandlerFromRuntimeStack(int32_t* node);
int newError(int32_t* node);
int newInt(int32_t value, int32_t* node);
int newBool(int32_t value, int32_t* node);
int newOperandStack(int32_t max_stack_size, int32_t* node);
void push(int32_t value);
int32_t pop(void);
int newStackFrame(int32_t pc, int32_t exception, int32_t* node);
int newEnvironment(int32_t size, int32_t* node);
int extendEnvironment(int32_t env, int32_t by, int32_t* node);
int32_t getValue(int32_t env, int32_t var);
void setValue(int32_t env, int32_t var, int32_t value);
int newClosure(int32_t addr, int32_t max_stack_size, int32_t* node);
void printMemory(void);
void flip(void);
int32_t copy(int32_t old);
int allocate(int32_t size, int32_t* new_node);
void printMemoryLocation(int32_t location);

// Error codes.

#define OK 0
#define MEMORY_ERROR 1
#define STACK_OVERFLOW 2
#define UNKNOWN_INSTRUCTION 3
#define NOT_IMPLEMENTED 4
#define UNHANDLED_EXCEPTION_ERROR 5
#define STACK_UNDERFLOW 6

// Instruction sizes.

#define INSTRUCTION_SIZE_OPS 1
#define INSTRUCTION_SIZE_LD 2
#define INSTRUCTION_SIZE_LDF 2
#define INSTRUCTION_SIZE_JMP 2
#define INSTRUCTION_SIZE_START 2
#define INSTRUCTION_SIZE_RTN 1
#define INSTRUCTION_SIZE_CALL 2
#define INSTRUCTION_SIZE_PRINT 1
#define INSTRUCTION_SIZE_GET 1
#define INSTRUCTION_SIZE_TL 2
#define INSTRUCTION_SIZE_TRY 3
#define INSTRUCTION_SIZE_ENDTRY 1
#define INSTRUCTION_SIZE_THROW 2

// Opcodes.

#define OPCODE_DONE 0
#define OPCODE_LDCI 1
#define OPCODE_LDCB 2
#define OPCODE_LD 3
#define OPCODE_LDF 4
#define OPCODE_LDRF 5
#define OPCODE_PLUS 6
#define OPCODE_MINUS 7
#define OPCODE_TIMES 8
#define OPCODE_DIV 9
#define OPCODE_AND 10
#define OPCODE_OR 11
#define OPCODE_NOT 12
#define OPCODE_LESS 13
#define OPCODE_GREATER 14
#define OPCODE_EQUAL 15
#define OPCODE_JOFR 16
#define OPCODE_GOTOR 17
#define OPCODE_CALL 18
#define OPCODE_RTN 19
#define OPCODE_START 20
#define OPCODE_TAILCALL 21
#define OPCODE_PRINTINT 22
#define OPCODE_PRINTBOOL 23
#define OPCODE_GETINT 24
#define OPCODE_GETBOOL 25
#define OPCODE_LDCF 26
#define OPCODE_PLUSFLOAT 27
#define OPCODE_MINUSFLOAT 28
#define OPCODE_TIMESFLOAT 29
#define OPCODE_DIVFLOAT 30
#define OPCODE_LESSFLOAT 31
#define OPCODE_GREATERFLOAT 32
#define OPCODE_EQUALFLOAT 33
#define OPCODE_GETFLOAT 34
#define OPCODE_PRINTFLOAT 35
#define OPCODE_CRTP 36
#define OPCODE_PROJ 37
#define OPCODE_EMPTY 38
#define OPCODE_INTTOFLOAT 39
#define OPCODE_FLOATTOINT 40
#define OPCODE_TRY 41
#define OPCODE_ENDTRY 42
#define OPCODE_THROW 43

// General structure of elements in memory.

#define SLOT_TAG 0
#define SLOT_SIZE 1
#define SLOT_FORWARD 2
#define SLOT_FIRST_CHILD 3
#define SLOT_LAST_CHILD 4