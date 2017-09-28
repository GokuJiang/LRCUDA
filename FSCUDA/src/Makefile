include config.mk

EXT ::= cu 

OBJS ::= LRCUDA.o

R_HOME ::= $(shell R RHOME)

#compiler/preprocessor options
INCS ::= -I. -I"$(CUDA_INC)" -I"$(R_INC)"
PARAMS ::= $(DEVICEOPTS) -Xcompiler $(CPICFLAGS)

#linker options
LD_PARAMS ::= $(DEVICEOPTS) -Xlinker '$(RPATH) $(R_FRAMEWORK)'
LIBS ::=  -L"$(R_LIB)" -L"$(CUDA_LIB)" -lcublas $(shell R CMD config BLAS_LIBS)

TARGETS ::= LRCUDA.so

NVCC ::= $(CUDA_HOME)/bin/nvcc -gencode arch=compute_20,code=sm_20 -gencode arch=compute_30,code=sm_30 -gencode arch=compute_35,code=sm_35 -gencode arch=compute_50,code=sm_50 -gencode arch=compute_52,code=sm_52 
#NVCC := $(CUDA_HOME)/bin/nvcc -gencode arch=compute_10,code=sm_10 

all: $(TARGETS) 

$(TARGETS): $(OBJS)
	$(NVCC) -shared $(LD_PARAMS) $(LIBS) $(OBJS) -o $@

$(OBJS): %.o: %.$(EXT)
	$(NVCC) -c  $(INCS) $(PARAMS) $^ -o $@

clean:
	rm -rf *o

.PHONY: all clean
