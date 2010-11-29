###
# Makefile 
###

CXX = g++
CPPFLAGS = -g -Wall -O3 -fopenmp
LDFLAGS = 
DEBUGFLAGS = -D DEBUG
PRODFLAGS = 

UTIL_SRCS = Argument_helper.cpp
UTIL_OBJS = $(UTIL_SRCS:.cpp=.o)


ERT_SRCS = ERT.cpp Histogram.cpp DecisionTree.cpp
ERT_OBJS = $(ERT_SRCS:.cpp=.o)
ERT_HDRS = $(ERT_SRCS:.cpp=.h)


SRCS = $(ERT_SRCS) $(UTIL_SRCS)
HDRS = $(ERT_HDRS) 

EXECUTABLES = ert 

ifeq ($(DEBUG),1)
CPPFLAGS += $(DEBUGFLAGS)
else
CPPFLAGS += $(PRODFLAGS)
endif

default: $(EXECUTABLES)

debug: debugflags $(EXECUTABLES)

debugflags:
	DEBUGFLAGS = -D DEBUG

ert : dependencies $(ERT_OBJS) $(UTIL_OBJS) 
	$(CXX) $(CPPFLAGS) -o $@ $(ERT_OBJS) $(UTIL_OBJS) 


# The dependencies below make use of make's default rules,
# under which a .o automatically depends on its .c and
# the action taken uses the $(CXX) and $(CPPFLAGS) variables.
# These lines describe a few extra dependencies involved.

dependencies:: $(SRCS) $(HDRS)
	$(CXX) $(CPPFLAGS) -MM $(SRCS) > Makefile.dependencies

-include Makefile.dependencies

clean :
	\rm -fr $(EXECUTABLES) Makefile.dependencies core a.out *.o *~	