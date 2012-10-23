#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "ratvm.h"

/*
 * MEMORY HEAP
 *
 * The memory is managed using Cheney's algorithm.
 */

#define MEMORY_SIZE (1000)
#define SPACE_SIZE (MEMORY_SIZE / 2)

int32_t memory[MEMORY_SIZE];  // The heap, where everything is stored.
int32_t mem_pos = 0;  // The position of free memory.
int32_t to_space = 0;  // We start writing in the bottom half.
int32_t top_of_space = SPACE_SIZE - 1; 
int32_t from_space = SPACE_SIZE;

// Address of the Operand Stack.
int32_t os;

// Address of the Environment.
int32_t e;

// A register, -1 if not used.
int32_t reg = -1;

#define RUNTIME_STACK_SIZE (20)

#define SLOT_CATCH_STACK_FRAME 6

/*
 * RUNTIME STACK
 *
 * The runtime stack is kept separated from memory,
 * but the stack frames themselves are on the heap. 
 */

// Runtime Stack
int32_t rs[RUNTIME_STACK_SIZE];
int32_t top_of_runtime_stack = 0;

// Pushes an element on top of the runtime stack.
// This element should be the address of a stack frame in the heap.
// Returns an error code.
int pushOnRuntimeStack(int32_t value) {
	if(top_of_runtime_stack >= RUNTIME_STACK_SIZE) {
		return STACK_OVERFLOW;
	}
	rs[top_of_runtime_stack] = value;
	top_of_runtime_stack += 1;
	return OK;
}

// Pops the first nonhandler and all previous handlers from the runtime stack.
int popFromRuntimeStack(int32_t* node) {
	int32_t frame;
	do {
		top_of_runtime_stack -= 1;
		if(top_of_runtime_stack < 0) {
			return STACK_UNDERFLOW;
		}
		frame = rs[top_of_runtime_stack];
	} while(memory[frame+SLOT_CATCH_STACK_FRAME] != 0);

	*node = frame;
	return OK;
}

// Pops the first handler and all previous nonhandlers from the runtime stack.
int popHandlerFromRuntimeStack(int32_t* node){
	int32_t frame;
	do {
		top_of_runtime_stack -= 1;
		if(top_of_runtime_stack < 0) {
			return UNHANDLED_EXCEPTION_ERROR;
		}
		frame = rs[top_of_runtime_stack];
	} while(memory[frame+SLOT_CATCH_STACK_FRAME] == 0);

	*node = frame;
	return OK;
}

// Pops the first handler for the exception and all previous stack frames from the runtime stack.
int popSpecificHandlerFromRuntimeStack(int32_t exception, int32_t* node){
	int32_t frame;
	do {
		top_of_runtime_stack -= 1;
		if(top_of_runtime_stack < 0) {
			return UNHANDLED_EXCEPTION_ERROR;
		}
		frame = rs[top_of_runtime_stack];
	} while(memory[frame+SLOT_CATCH_STACK_FRAME] != exception);

	*node = frame;
	return OK;
}

// Integers.

#define SLOT_INT_VALUE 5
#define TAG_INT 1
#define SIZE_INT 6
#define FIRST_CHILD_INT 5
#define LAST_CHILD_INT 4

// Allocates an integer value in memory.
int newInt(int32_t value, int32_t* node) {

	int err;

	if((err = allocate(SIZE_INT, node)) != OK) {
		return err;
	}

	memory[*node+SLOT_TAG] = TAG_INT;
	memory[*node+SLOT_SIZE] = SIZE_INT;
	memory[*node+SLOT_FIRST_CHILD] = FIRST_CHILD_INT;
	memory[*node+SLOT_LAST_CHILD] = LAST_CHILD_INT;
	memory[*node+SLOT_FORWARD] = -1;
	memory[*node+SLOT_INT_VALUE] = value;

	return OK;
}

// Floats.

#define SLOT_FLOAT_VALUE 5
#define TAG_FLOAT 7
#define SIZE_FLOAT 6
#define FIRST_CHILD_FLOAT 5
#define LAST_CHILD_FLOAT 4

// Allocates a float value in memory.
int newFloat(int32_t value, int32_t* node) {

	int err;

	if((err = allocate(SIZE_INT, node)) != OK) {
		return err;
	}

	memory[*node+SLOT_TAG] = TAG_FLOAT;
	memory[*node+SLOT_SIZE] = SIZE_FLOAT;
	memory[*node+SLOT_FIRST_CHILD] = FIRST_CHILD_FLOAT;
	memory[*node+SLOT_LAST_CHILD] = LAST_CHILD_FLOAT;
	memory[*node+SLOT_FORWARD] = -1;
	memory[*node+SLOT_FLOAT_VALUE] = value;

	return OK;
}

// Booleans.

#define SLOT_BOOL_VALUE 5
#define TAG_BOOL 2
#define SIZE_BOOL 6
#define FIRST_CHILD_BOOL 5
#define LAST_CHILD_BOOL 4

#define VALUE_TRUE 1
#define VALUE_FALSE 0

// Allocates a boolean value in memory.
int newBool(int32_t value, int32_t* node) {

	int err;

	if((err = allocate(SIZE_BOOL, node)) != OK) {
		return err;
	}

	memory[*node+SLOT_TAG] = TAG_BOOL;
	memory[*node+SLOT_SIZE] = SIZE_BOOL;
	memory[*node+SLOT_FIRST_CHILD] = FIRST_CHILD_BOOL;
	memory[*node+SLOT_LAST_CHILD] = LAST_CHILD_BOOL;
	memory[*node+SLOT_FORWARD] = -1;
	memory[*node+SLOT_BOOL_VALUE] = value;

	return OK;
}

// Tuples.

#define TAG_TUPLE 8
#define BASE_SIZE_TUPLE 5
#define FIRST_CHILD_TUPLE 5
#define BASE_LAST_CHILD_TUPLE 4

// Allocates a tuple in memory.
int newTuple(int32_t size, int32_t* node) {

	int err;

	if((err = allocate(BASE_SIZE_TUPLE + size, node)) != OK) {
		return err;
	}

	memory[*node+SLOT_TAG] = TAG_TUPLE;
	memory[*node+SLOT_SIZE] = BASE_SIZE_TUPLE + size;
	memory[*node+SLOT_FIRST_CHILD] = FIRST_CHILD_TUPLE;
	memory[*node+SLOT_LAST_CHILD] = BASE_LAST_CHILD_TUPLE + size;
	memory[*node+SLOT_FORWARD] = -1;

	return OK;
}

// Returns the element at position var starting at the end.
int32_t getElement(int32_t tuple, int32_t var) {
	if(var < 0 || var > memory[tuple+SLOT_LAST_CHILD] - memory[tuple+SLOT_FIRST_CHILD]) {
		printf("Get element out of range");
		fflush(stdout);
	}
	return memory[tuple+memory[tuple+SLOT_LAST_CHILD]-var];
}

// Set the element at position var starting at the end to the value.
void setElement(int32_t tuple, int32_t var, int32_t value) {
	if(var < 0 || var > memory[tuple+SLOT_LAST_CHILD] - memory[tuple+SLOT_FIRST_CHILD]) {
		printf("Set element out of range");
		fflush(stdout);
	}
	memory[tuple+memory[tuple+SLOT_LAST_CHILD]-var] = value;
}

// Operand Stack

#define TAG_OPERAND_STACK 3
#define BASE_SIZE_OPERAND_STACK 5
#define FIRST_CHILD_OPERAND_STACK 5
#define BASE_LAST_CHILD_OPERAND_STACK 4

// Allocates an operand stack in memory.
int newOperandStack(int32_t max_stack_size, int32_t* node) {
	int err;

	if((err = allocate(BASE_SIZE_OPERAND_STACK + max_stack_size, node)) != OK) {
		return err;
	}

	memory[*node+SLOT_TAG] = TAG_OPERAND_STACK;
	memory[*node+SLOT_SIZE] = BASE_SIZE_OPERAND_STACK + max_stack_size;
	memory[*node+SLOT_FIRST_CHILD] = FIRST_CHILD_OPERAND_STACK;
	memory[*node+SLOT_LAST_CHILD] = BASE_LAST_CHILD_OPERAND_STACK;
	memory[*node+SLOT_FORWARD] = -1;

	return OK;
}

// Pushes a value on the current operand stack.
void push(int32_t value) {
	memory[os+SLOT_LAST_CHILD] += 1;
	if(memory[os+SLOT_LAST_CHILD] - memory[os+SLOT_FIRST_CHILD] > memory[os+SLOT_SIZE] - BASE_SIZE_OPERAND_STACK) {
		printf("Push out of range.\n");
		fflush(stdout);
	}
	memory[os+memory[os+SLOT_LAST_CHILD]] = value;
}

// Pops an element from the current operand stack.
int32_t pop() {
	if(memory[os+SLOT_LAST_CHILD] < memory[os+SLOT_FIRST_CHILD]) {
		printf("Pop out of range.\n");
		fflush(stdout);
	}
	int32_t value = memory[os+memory[os+SLOT_LAST_CHILD]];
	memory[os+SLOT_LAST_CHILD] -= 1;
	return value;
}

// Stack Frame

#define TAG_STACK_FRAME 4
#define SIZE_STACK_FRAME 9
#define SLOT_ADDRESS_STACK_FRAME 5
#define FIRST_CHILD_STACK_FRAME 7
#define LAST_CHILD_STACK_FRAME 8
#define SLOT_ENVIRONMENT_STACK_FRAME 7
#define SLOT_OPERAND_STACK_STACK_FRAME 8

// Allocates a stack frame in memory.
int newStackFrame(int32_t pc, int32_t exception, int32_t* node) {
	int err;

	if((err = allocate(SIZE_STACK_FRAME, node)) != OK) {
		return err;
	}

	memory[*node+SLOT_TAG] = TAG_STACK_FRAME;
	memory[*node+SLOT_SIZE] = SIZE_STACK_FRAME;
	memory[*node+SLOT_FIRST_CHILD] = FIRST_CHILD_STACK_FRAME;
	memory[*node+SLOT_LAST_CHILD] = LAST_CHILD_STACK_FRAME;
	memory[*node+SLOT_FORWARD] = -1;
	memory[*node+SLOT_ADDRESS_STACK_FRAME] = pc;
	memory[*node+SLOT_OPERAND_STACK_STACK_FRAME] = os;
	memory[*node+SLOT_ENVIRONMENT_STACK_FRAME] = e;
	memory[*node+SLOT_CATCH_STACK_FRAME] = exception;

	return OK;
}

// Environment

#define TAG_ENVIRONMENT 5
#define BASE_SIZE_ENVIRONMENT 5
#define FIRST_CHILD_ENVIRONMENT 5
#define BASE_LAST_CHILD_ENVIRONMENT 4

// Allocates an environment in memory.
int newEnvironment(int32_t size, int32_t* node) {

	int err;

	if((err = allocate(BASE_SIZE_ENVIRONMENT + size, node)) != OK) {
		return err;
	}

	memory[*node+SLOT_TAG] = TAG_ENVIRONMENT;
	memory[*node+SLOT_SIZE] = BASE_SIZE_ENVIRONMENT + size;
	memory[*node+SLOT_FIRST_CHILD] = FIRST_CHILD_ENVIRONMENT;
	memory[*node+SLOT_LAST_CHILD] = BASE_LAST_CHILD_ENVIRONMENT + size;
	memory[*node+SLOT_FORWARD] = -1;


	return OK;
}

// Extends an environment by a certain number of elements.
int extendEnvironment(int32_t env, int32_t by, int32_t* node) {


	int err;
	int32_t old_size = memory[env+SLOT_SIZE] - BASE_SIZE_ENVIRONMENT;

	if((err = newEnvironment(old_size+by, node)) != OK) {
		return err;
	}

	int32_t i;
	if(memory[env+SLOT_FORWARD] != -1) {
		env = memory[env+SLOT_FORWARD];
	}

	for(i = memory[env+SLOT_FIRST_CHILD]; i<=memory[env+SLOT_LAST_CHILD]; i++) {
		memory[*node+i] = memory[env+i];
	}


	return OK;
}

// Returns the value at position var starting at the end.
int32_t getValue(int32_t env, int32_t var) {
	if(var < 0 || var > memory[env+SLOT_LAST_CHILD] - memory[env+SLOT_FIRST_CHILD]) {
		printf("Get value out of range");
		fflush(stdout);
	}
	return memory[env+memory[env+SLOT_LAST_CHILD]-var];
}

// Set the value at position var starting at the end to the value.
void setValue(int32_t env, int32_t var, int32_t value) {
	if(var < 0 || var > memory[env+SLOT_LAST_CHILD] - memory[env+SLOT_FIRST_CHILD]) {
		printf("Set value out of range");
		fflush(stdout);
	}
	memory[env+memory[env+SLOT_LAST_CHILD]-var] = value;
}

// Closure

#define TAG_CLOSURE 6
#define SIZE_CLOSURE 8
#define FIRST_CHILD_CLOSURE 7
#define LAST_CHILD_CLOSURE 7
#define SLOT_ADDRESS_CLOSURE 5
#define SLOT_STACK_SIZE_CLOSURE 6
#define SLOT_ENVIRONMENT_CLOSURE 7

// Allocates a new closure in memory.
int newClosure(int32_t addr, int32_t max_stack_size, int32_t* node) {
	int err;

	if((err = allocate(SIZE_CLOSURE, node)) != OK) {
		return err;
	}

	memory[*node+SLOT_TAG] = TAG_CLOSURE;
	memory[*node+SLOT_SIZE] = SIZE_CLOSURE;
	memory[*node+SLOT_FIRST_CHILD] = FIRST_CHILD_CLOSURE;
	memory[*node+SLOT_LAST_CHILD] = LAST_CHILD_CLOSURE;
	memory[*node+SLOT_FORWARD] = -1;
	memory[*node+SLOT_ADDRESS_CLOSURE] = addr;
	memory[*node+SLOT_STACK_SIZE_CLOSURE] = max_stack_size;
	memory[*node+SLOT_ENVIRONMENT_CLOSURE] = e;

	return OK;
}

/*
 * MEMORY MANAGEMENT
 *
 * The heap is managed using Cheney's algorithm.
 */


// Flips between the two spaces of memory,
// Copying only live nodes.
void flip() {

	int32_t temp = from_space;
	from_space = to_space;
	to_space = temp;
	top_of_space = to_space + SPACE_SIZE - 1;


	int32_t scan = to_space;
	mem_pos = to_space;

	e = copy(e);

	os = copy(os);

	int32_t i;
	for(i=0; i<top_of_runtime_stack; i++) {
		rs[i] = copy(rs[i]);
	}

	// If we have registered something
	// that would otherwise have been consided dead.
	if(reg != -1) {
		reg = copy(reg);
	}

	// Breadth first traversal of live memory.
	while(scan < mem_pos) {
		temp = memory[scan+SLOT_SIZE];
		for(i=memory[scan+SLOT_FIRST_CHILD]; i<=memory[scan+SLOT_LAST_CHILD]; i++) {
				memory[scan+i] = copy(memory[scan+i]);
		}
		scan += temp;
	}
}

// Copies a node from old memory to new memory,
// if necessary. Returns the address of the node
// in the new memory.
int32_t copy(int32_t old) {


	if(memory[old + SLOT_FORWARD] >= to_space && memory[old + SLOT_FORWARD] <= top_of_space) {
		return memory[old + SLOT_FORWARD];
	} else {
		int32_t addr = mem_pos;
		int32_t i;
		int32_t s = memory[old+SLOT_SIZE];
		for(i=0; i<s; ++i){
			memory[addr+i] = memory[old+i];
		}

		memory[old+SLOT_FORWARD] = addr;
		mem_pos += s;

		return addr;
	}
}

// Allocates memory for a node.
int allocate(int32_t size, int32_t* new_node) {

	if(size + mem_pos > top_of_space) {
		flip();
	}

	if(size + mem_pos > top_of_space) {
		return MEMORY_ERROR;
	}
	int i;
	*new_node = mem_pos;
	for(i=0; i<size; i++) {
		memory[*new_node+i] = -1;
	}
	mem_pos += size;

	return OK;
}

// Prints the memory, for debugging purpose.
void printMemory() {
	int32_t x = to_space;
	int32_t s;
	while(x < mem_pos && x < top_of_space) {
		s = memory[x+SLOT_SIZE];
		printf("Location %i : \n", x);
		printMemoryLocation(x);
		x += s;
	}
}

// Prints a specific memory location, for debugging purpose.
void printMemoryLocation(int32_t location) {
	int i;
	switch(memory[location+SLOT_TAG]) {
		case TAG_INT:
			printf("%i\n", memory[location+SLOT_INT_VALUE]);
			break;

		case TAG_BOOL:
			if(memory[location+SLOT_BOOL_VALUE] == 0) {
				printf("false\n");
			}
			else{
				printf("true\n");
			}
			break;

		case TAG_CLOSURE:
			printf("fun ... -> ... end\n");
			printf("Environment: %i\n", memory[location+SLOT_ENVIRONMENT_CLOSURE]);
			break;

		case TAG_ENVIRONMENT:
			printf("Environment\n");
			printf("Size: %i\n", memory[location+SLOT_SIZE]);
			printf("Children: %i\n", 1 + memory[location+SLOT_LAST_CHILD] - memory[location+SLOT_FIRST_CHILD]);

			for(i=memory[location+SLOT_FIRST_CHILD]; i<=memory[location+SLOT_LAST_CHILD]; i++) {
				printf("%i\n", memory[location+i]);
			}

			printf(".............\n");
			break;

		case TAG_OPERAND_STACK:
			printf("Operand stack\n");
			printf("Size: %i\n", memory[location+SLOT_SIZE]);
			printf("First child: %i\n", memory[location+SLOT_FIRST_CHILD]);
			printf("Last child: %i\n", memory[location+SLOT_LAST_CHILD]);
			break;

		case TAG_STACK_FRAME:
			printf("Stack Frame\n");
			printf("Size: %i\n", memory[location+SLOT_SIZE]);
			printf("First child: %i\n", memory[location+SLOT_FIRST_CHILD]);
			printf("Last child: %i\n", memory[location+SLOT_LAST_CHILD]);
			printf("Environment: %i\n", memory[location+SLOT_ENVIRONMENT_STACK_FRAME]);
			break;

		case TAG_FLOAT:
			printf("%f\n", *(float*)&memory[location+SLOT_FLOAT_VALUE]);
			break;

		default:
			printf("Unknown value\n");
			break;
	}
}


// Program counter.
int32_t pc = 0;


// Exceptions.

#define EXCEPTION_DIV_BY_ZERO 1
#define EXCEPTION_NO_READ 2
#define EXCEPTION_EOF 3

// Throws an exception with exception number specified.
int throw(int32_t exception) {

	int32_t handler;
	int32_t error_code;

	if((error_code = popSpecificHandlerFromRuntimeStack(exception, &handler)) != OK) {
		return error_code;
	}

	os = memory[handler+SLOT_OPERAND_STACK_STACK_FRAME];
	e = memory[handler+SLOT_ENVIRONMENT_STACK_FRAME];
	pc = memory[handler+SLOT_ADDRESS_STACK_FRAME];

	return OK;
}


/*
 * MAIN
 */

int main(int argc, char **argv) {

	// Setting up everything.

	if(argc != 2) {
		fprintf(stderr, "Must provide a single filename as argument.");
		return 1;
	}

	FILE* file = fopen(argv[1], "rb");

	if(file == NULL) {
		fprintf(stderr, "Unable to open file %s.", argv[1]);
		return 1;
	}
	
	fseek(file, 0, SEEK_END);

	unsigned long size = ftell(file);

	fseek(file, 0, SEEK_SET);

	int32_t* instructions = (int32_t*)malloc(size+1);

	if(instructions == NULL) {
		fprintf(stderr, "Can not load file to memory.");
		fclose(file);
		return 1;
	}

	fread(instructions, size, 1, file);
	fclose(file);

	int length = size / 4;

	// Current instruction.
	int32_t instruction;

	// Error code.
	int32_t error_code = OK;

	// Setting base environment.
	error_code = newEnvironment(0, &e);

	while(error_code == OK) {

		// Getting the instruction.
		instruction = instructions[pc];

		switch(instruction) {
			case OPCODE_START:  // Start of the program.
				if((error_code = newOperandStack(instructions[pc+1], &os)) != OK) {
					break;
				}
				pc += INSTRUCTION_SIZE_START;
				break;

			case OPCODE_DONE:  // End of the program.
				free(instructions);
				instructions = NULL;
				return 0;

			case OPCODE_LDCI: {  // Load constant integer.
				int32_t result;

				if((error_code = newInt(instructions[pc+1], &result)) != OK) {
					break;
				}
				push(result);
				pc += INSTRUCTION_SIZE_LD;
				break; }

			case OPCODE_LDCF: {  // Load constant float.
				int32_t result;

				if((error_code = newFloat(instructions[pc+1], &result)) != OK) {
					break;
				}
				push(result);
				pc += INSTRUCTION_SIZE_LD;
				break; }

			case OPCODE_LDCB: {  // Load constant boolean.
				int32_t result;

				if((error_code = newBool(instructions[pc+1], &result)) != OK) {
					break;
				}
				push(result);
				pc += INSTRUCTION_SIZE_LD;
				break; }

			case OPCODE_EQUAL: {  // Equal between integer.
				int32_t a = memory[pop()+SLOT_INT_VALUE];
				int32_t b = memory[pop()+SLOT_INT_VALUE];
				int32_t result;

				if((error_code = newBool(a == b ? VALUE_TRUE : VALUE_FALSE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_LESS:{  // Less between integer.
				int32_t a = memory[pop()+SLOT_INT_VALUE];
				int32_t b = memory[pop()+SLOT_INT_VALUE];
				int32_t result;

				if((error_code = newBool(b < a ? VALUE_TRUE : VALUE_FALSE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_GREATER: {  // Greater between integer.
				int32_t a = memory[pop()+SLOT_INT_VALUE];
				int32_t b = memory[pop()+SLOT_INT_VALUE];
				int32_t result;

				if((error_code = newBool(b > a ? VALUE_TRUE : VALUE_FALSE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_PLUS: {  // Plus between integer.
				int32_t result;

				if((error_code = newInt(memory[pop()+SLOT_INT_VALUE] + memory[pop()+SLOT_INT_VALUE], &result)) != OK) {
					break;
				}
				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_MINUS: {  // Minus between integer.
				int32_t a = memory[pop()+SLOT_INT_VALUE];
				int32_t b = memory[pop()+SLOT_INT_VALUE];
				int32_t result;

				if((error_code = newInt(b - a, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_TIMES: {  // Multiplication between integer.
				int32_t result;

				if((error_code = newInt(memory[pop()+SLOT_INT_VALUE] * memory[pop()+SLOT_INT_VALUE], &result)) != OK) {
					break;
				}
				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_DIV: {  // Division between integer.
				int32_t a = memory[pop()+SLOT_INT_VALUE];
				int32_t b = memory[pop()+SLOT_INT_VALUE];
				int32_t result;

				if(a == 0) {
					error_code = throw(EXCEPTION_DIV_BY_ZERO);
					break;
				}

				if((error_code = newInt(b / a, &result)) != OK) {
					break;
				}
				
				push(result);
					
				pc += INSTRUCTION_SIZE_OPS;

				break; }

			case OPCODE_EQUALFLOAT: {  // Equal between float.
				float a = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				float b = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				int32_t result;

				if((error_code = newBool(a == b ? VALUE_TRUE : VALUE_FALSE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_LESSFLOAT:{  // Less between float.
				float a = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				float b = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				int32_t result;

				if((error_code = newBool(b < a ? VALUE_TRUE : VALUE_FALSE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_GREATERFLOAT: {  // Greater between float.
				float a = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				float b = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				int32_t result;

				if((error_code = newBool(b > a ? VALUE_TRUE : VALUE_FALSE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_PLUSFLOAT: {  // Plus between float.
				float a = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				float b = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				int32_t result;
				float x = a + b;
				if((error_code = newFloat(*(int32_t *)&x, &result)) != OK) {
					break;
				}
				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_MINUSFLOAT: {  // Minus between float.
				float a = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				float b = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				int32_t result;
				float x = b - a;
				if((error_code = newFloat(*(int32_t *)&x, &result)) != OK) {
					break;
				}
				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_TIMESFLOAT: {  // Multiplication between float.
				float a = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				float b = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				int32_t result;
				float x = a * b;
				if((error_code = newFloat(*(int32_t *)&x, &result)) != OK) {
					break;
				}
				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_DIVFLOAT: {  // Division between float.
				float a = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				float b = *(float *)&memory[pop()+SLOT_FLOAT_VALUE];
				int32_t result;

				if(a == 0.0) {
					error_code = throw(EXCEPTION_DIV_BY_ZERO);
					break;
				}

				float x = b / a; 
				if((error_code = newFloat(*(int32_t *)&x, &result)) != OK) {
					break;
				}

				push(result);

				pc += INSTRUCTION_SIZE_OPS;

				break; }

			case OPCODE_AND: {  // And between boolean.
				int32_t a = memory[pop()+SLOT_BOOL_VALUE];
				int32_t b = memory[pop()+SLOT_BOOL_VALUE];
				int32_t result;

				if((error_code = newBool(((b == VALUE_TRUE) && (a == VALUE_TRUE)) ? VALUE_TRUE : VALUE_FALSE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_OR: {  // Or between boolean.
				int32_t a = memory[pop()+SLOT_BOOL_VALUE];
				int32_t b = memory[pop()+SLOT_BOOL_VALUE];
				int32_t result;

				if((error_code = newBool(((b == VALUE_TRUE) || (a == VALUE_TRUE)) ? VALUE_TRUE : VALUE_FALSE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_NOT: {  // Not a boolean.
				int32_t result;
				if((error_code = newBool((memory[pop()+SLOT_BOOL_VALUE] == VALUE_TRUE) ? VALUE_FALSE : VALUE_TRUE, &result)) != OK) {
					break;
				}

				push(result);
				pc += INSTRUCTION_SIZE_OPS;
				break; }

			case OPCODE_INTTOFLOAT: {  // Casting for int to float.
				int32_t integer = pop();
				float value = (float)memory[integer+SLOT_INT_VALUE];
				int32_t floating;

				if((error_code = newFloat(*(int32_t *)&value, &floating)) != OK) {
					break;
				}

				push(floating);

				pc += INSTRUCTION_SIZE_OPS;

				break;
			}

			case OPCODE_FLOATTOINT: {  // Casting from float to int.
				int32_t floating = pop();
				int32_t value = (int32_t)(*(float *)&memory[floating+SLOT_FLOAT_VALUE]);
				int32_t integer;

				if((error_code = newFloat(*(int32_t *)&value, &integer)) != OK) {
					break;
				}

				push(integer);

				pc += INSTRUCTION_SIZE_OPS;

				break;
			}

			case OPCODE_CRTP: {  // Creation of tuple.
				int32_t size = instructions[pc+1];
				int32_t tuple;

				if((error_code = newTuple(size, &tuple)) != OK) {
					break;
				}

				int32_t i;
				int32_t element;
				for(i=size-1; i>=0; i--) {
					element = pop();
					setElement(tuple, i, element);
				}

				push(tuple);
				pc += INSTRUCTION_SIZE_TL;
				break; }

			case OPCODE_PROJ: {  // Projection of tuple
				int32_t index = instructions[pc+1];
				int32_t tuple = pop();
				push(getElement(tuple, index));
				pc += INSTRUCTION_SIZE_TL;
				break;
			}

			case OPCODE_EMPTY: {  // Checks if a tuple is empty.
				int32_t tuple = pop();
				int32_t n = memory[tuple+SLOT_LAST_CHILD] - memory[tuple+SLOT_FIRST_CHILD] + 1;
				int32_t boolean;
				if((error_code=newBool(n == 0 ? VALUE_TRUE : VALUE_FALSE, &boolean)) != OK) {
					break;
				}
				push(boolean);
				pc += INSTRUCTION_SIZE_OPS;
				break;
			}

			case OPCODE_GOTOR:  // Goto a relative position.
				pc += instructions[pc+1];
				break;

			case OPCODE_JOFR:  // Jump on false to a relative position.
				if(memory[pop()+SLOT_BOOL_VALUE] == VALUE_FALSE) {
					pc += instructions[pc+1];
				}
				else {
					pc += INSTRUCTION_SIZE_JMP;
				}
				break;

			case OPCODE_LD:  // Load a variable.
				push(getValue(e, instructions[pc+1]));
				pc += INSTRUCTION_SIZE_LD;
				break;

			case OPCODE_LDF: {  // Load a function definition.
				int32_t stack_frame_size = instructions[pc+1];

				int32_t closure;

				if((error_code = newClosure(pc+INSTRUCTION_SIZE_JMP+INSTRUCTION_SIZE_LDF, stack_frame_size, &closure)) != OK) {
					break;
				}

				push(closure);

				pc += INSTRUCTION_SIZE_LDF;
				break; }

			case OPCODE_LDRF: {  // Load a recursive function definition.
				int32_t stack_frame_size = instructions[pc+1];


				if((error_code = newClosure(pc+INSTRUCTION_SIZE_JMP+INSTRUCTION_SIZE_LDF, stack_frame_size, &reg)) != OK) {
					break;
				}

				push(reg);

				int32_t new_environment;

				if((error_code = extendEnvironment(e, 1, &new_environment)) != OK) {
					break;
				}

				memory[reg+SLOT_ENVIRONMENT_CLOSURE] = new_environment;
				memory[new_environment+memory[new_environment+SLOT_LAST_CHILD]] = reg;

				reg = -1;
				pc += INSTRUCTION_SIZE_LDF;
				break; }

			case OPCODE_TAILCALL: {  // Tail call.

				int32_t a = instructions[pc+1];  // Number of arguments
				int32_t b = memory[os+memory[os+SLOT_LAST_CHILD]-a];  // Closure
				int32_t c = memory[b+SLOT_ADDRESS_CLOSURE]; // Address of the closure
				int32_t d = memory[b+SLOT_STACK_SIZE_CLOSURE]; // Stack Size

				if((error_code = extendEnvironment(memory[b+SLOT_ENVIRONMENT_CLOSURE], a, &e)) != OK) {
					break;
				}

				int32_t i = 0;
				int32_t value;
				while(i < a) {
					value = pop();
					setValue(e, i, value);
					i += 1;
				}
				pop();

				pc = c;

				if((error_code = newOperandStack(d, &os)) != OK) {
					break;
				}
				
				break; }

				

			case OPCODE_CALL: {  // Call of a function.

				int32_t a = instructions[pc+1];  // Number of arguments
				int32_t b = memory[os+memory[os+SLOT_LAST_CHILD]-a];  // Closure
				int32_t c = memory[b+SLOT_ADDRESS_CLOSURE]; // Address of the closure
				int32_t d = memory[b+SLOT_STACK_SIZE_CLOSURE]; // Stack Size

				if((error_code = extendEnvironment(memory[b+SLOT_ENVIRONMENT_CLOSURE], a, &reg)) != OK) {
					break;
				}

				int32_t i = 0;
				int32_t value;
				while(i < a) {
					value = pop();
					setValue(reg, i, value);
					i += 1;
				}
				pop();

				int32_t new_stack_frame;

				if((error_code = newStackFrame(pc + INSTRUCTION_SIZE_CALL, 0, &new_stack_frame)) != OK) {
					break;
				}

				if((error_code = pushOnRuntimeStack(new_stack_frame)) != OK) {
					break;
				}

				pc = c;
				e = reg;
				reg = -1;

				if((error_code = newOperandStack(d, &os)) != OK) {
					break;
				}
				
				break; }

			case OPCODE_RTN: {  // Return from a function.

				int32_t stack_frame;

				if((error_code = popFromRuntimeStack(&stack_frame)) != OK) {
					break;
				}

				pc = memory[stack_frame+SLOT_ADDRESS_STACK_FRAME];
				e = memory[stack_frame+SLOT_ENVIRONMENT_STACK_FRAME];

				int32_t result = pop();

				os = memory[stack_frame+SLOT_OPERAND_STACK_STACK_FRAME];

				push(result);
				break; }

			case OPCODE_PRINTINT: {  // Print an integer.
				int32_t integer = pop();
				printf("Result : %i\n", memory[integer+SLOT_INT_VALUE]);
				push(integer);
				fflush(stdout);
				pc += INSTRUCTION_SIZE_PRINT;
				break; }

			case OPCODE_PRINTFLOAT: {  // Print a float.
				int32_t floating = pop();
				printf("Result : %g\n", *(float *)&memory[floating+SLOT_FLOAT_VALUE]);
				push(floating);
				fflush(stdout);
				pc += INSTRUCTION_SIZE_PRINT;
				break; }

			case OPCODE_PRINTBOOL: {  // Print a boolean.
				int32_t boolean = pop();
				if(memory[boolean+SLOT_BOOL_VALUE] == VALUE_TRUE) {
					printf("Result : true\n");
				}
				else {
					printf("Result : false\n");
				}
				push(boolean);
				fflush(stdout);
				pc += INSTRUCTION_SIZE_PRINT;
				break; }

			case OPCODE_GETINT: {  // Get an integer from standart input.

				int32_t input;
				int32_t integer;

				printf("Enter an integer : ");
				fflush(stdout);

				if(scanf("%i", &input)) {
					if((error_code = newInt(input, &integer)) != OK) {
						break;
					}
					else {
						push(integer);
						pc += INSTRUCTION_SIZE_GET;
					}
				}
				else {
					pc = length - 1;
				}

				break; }

			case OPCODE_GETFLOAT: {  // Get a float from standart input.

				float input;
				int32_t floating;

				printf("Enter a float : ");
				fflush(stdout);

				if(scanf("%f", &input)) {
					if((error_code = newFloat(*(int32_t *)&input, &floating)) != OK) {
						break;
					}
					else {
						push(floating);
						pc += INSTRUCTION_SIZE_GET;
					}
				}
				else {
					pc = length - 1;
				}

				break; }

			case OPCODE_GETBOOL: {  // Get a boolean from standart input.

				char a;

				printf("Enter y or n (yes or no) : ");
				fflush(stdout);

				while((a = fgetc(stdin)) == ' ' || a == '\n' || a == EOF);
				if (a == EOF) {
					pc = length - 1;
					break;
				}

				if(a == EOF) {
					pc = length - 1;
					break;
				}

				int32_t boolean;


				if((error_code = newBool(a == 'y' ? VALUE_TRUE : VALUE_FALSE, &boolean)) != OK) {
					break;
				}
				else {
					push(boolean);
					pc += INSTRUCTION_SIZE_GET;
				}


				break; }

			case OPCODE_TRY: {  // Enter a try block.
				int32_t exception = instructions[pc+1];

				int32_t rel_addr_catch = instructions[pc+2];

				int32_t frame;

				if((error_code = newStackFrame(pc+rel_addr_catch, exception, &frame)) != OK) {
					break;
				}

				if((error_code = pushOnRuntimeStack(frame)) != OK) {
					break;
				}

				int32_t new_os;
				int32_t s = memory[os+SLOT_SIZE];

				// Copy the operand stack.
				if((error_code = allocate(s, &new_os)) != OK) {
					break;
				}

				int i;
				for(i=0; i<s; i++) {
					memory[new_os+i] = memory[os+i];
				}

				os = new_os;

				pc += INSTRUCTION_SIZE_TRY;

				break;
			}

			case OPCODE_ENDTRY: {  // Leave a try block.
				int32_t frame;
				if((error_code = popHandlerFromRuntimeStack(&frame)) != OK) {
					break;
				}
				pc += INSTRUCTION_SIZE_ENDTRY;
				break;
			}

			case OPCODE_THROW: {  // Throw an exception.
				int32_t exception = instructions[pc+1];

				error_code = throw(exception);

				break;
			}

			default:  // Unknown instruction.
				error_code = UNKNOWN_INSTRUCTION;
				break;
		} 
	}

	// Freeing the memory used for instructions.
	free(instructions);
	instructions = NULL;

	switch(error_code) {
		case MEMORY_ERROR:
			fprintf(stderr, "Memory exhausted.\n");
			break;
		case STACK_OVERFLOW:
			fprintf(stderr, "Runtime stack overflow.\n");
			break;
		case UNKNOWN_INSTRUCTION:
			fprintf(stderr, "Unknown instruction.\n");
			break;
		case NOT_IMPLEMENTED:
			fprintf(stderr, "Instruction not implemented.\n");
			break;
		case STACK_UNDERFLOW:
			fprintf(stderr, "Runtime stack underflow.\n");
			break;
		case UNHANDLED_EXCEPTION_ERROR:
			fprintf(stderr, "Error, unhandled exception.\n");
			break;
		default:
			fprintf(stderr, "Unknown error.\n");
			break;
	}

	fflush(stderr);

	return 1;
}