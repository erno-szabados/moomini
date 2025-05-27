SRC_DIR = src
TEST_DIR = test
BIN_DIR = bin
GM2FLAGS = -fiso

GM2 = gm2

SRC_FILES = $(SRC_DIR)/UTF8.mod
DEF_FILES = $(SRC_DIR)/UTF8.def
TEST_FILES = $(TEST_DIR)/TestUTF8.mod

all: test

$(BIN_DIR)/UTF8.o: $(SRC_DIR)/UTF8.mod $(SRC_DIR)/UTF8.def | $(BIN_DIR)
	$(GM2) $(GM2FLAGS) -I$(SRC_DIR) -c $(SRC_DIR)/UTF8.mod -o $(BIN_DIR)/UTF8.o

$(BIN_DIR)/TestUTF8: $(BIN_DIR)/UTF8.o $(TEST_FILES) | $(BIN_DIR)
	$(GM2) $(GM2FLAGS) -I$(SRC_DIR) $(TEST_FILES) $(BIN_DIR)/UTF8.o -o $(BIN_DIR)/TestUTF8

test: $(BIN_DIR)/TestUTF8
	$(BIN_DIR)/TestUTF8

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

clean:
	rm -rf $(BIN_DIR)
	rm *.o

.PHONY: all clean test