CC=g++
LIBRARY=libdynetc.so
SRCS=dynet.cpp expr.cpp rnn.cpp trainer.cpp vector.cpp
OBJS=$(SRCS:.cpp=.o)
DEPS=$(SRCS:.cpp=.d)
CXXFLAGS= -std=c++11 -g -fpic -stdlib=libc++ -Wall -march=native -O2
INCLUDES=-I$(DYNET) -I$(EIGEN3_INCLUDE_DIR)
LIBDIR=-L$(DYNET)

all: $(LIBRARY) $(OBJS)

$(LIBRARY): $(OBJS)
	$(CC) $(CXXFLAGS) -ldynet -shared $(INCLUDES) -o $@ $(OBJS) $(LIBDIR)

.cpp.o:
	$(CC) $(CXXFLAGS) -c $< -MMD -MP $(INCLUDES)

.PHONY: clean
clean:
	rm $(LIBRARY) $(OBJS) $(DEPS)

-include $(DEPS)


