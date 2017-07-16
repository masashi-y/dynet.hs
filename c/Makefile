CC=g++
LIBRARY=libdynetc.so
SRCS=dynet.cpp expr.cpp rnn.cpp trainer.cpp vector.cpp
OBJS=$(SRCS:.cpp=.o)
DEPS=$(SRCS:.cpp=.d)
CXXFLAGS= -std=c++11 -g -fpic -Wall -march=native -O2
UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
	CXXFLAGS += -stdlib=libc++
endif
INCLUDES=-I$(DYNET) -I$(EIGEN3_INCLUDE_DIR)
LIBDIR=-L$(DYNET)/build/dynet

all: $(LIBRARY) $(OBJS)

$(LIBRARY): $(OBJS)
	$(CC) $(CXXFLAGS) -shared -o $@ $(OBJS) $(INCLUDES) $(LIBDIR) -ldynet

.cpp.o:
	$(CC) $(CXXFLAGS) -c $< -MMD -MP $(INCLUDES) $(LIBDIR) -ldynet

.PHONY: clean
clean:
	rm $(LIBRARY) $(OBJS) $(DEPS)

-include $(DEPS)


