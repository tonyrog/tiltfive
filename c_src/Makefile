#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
CFLAGS += -I$(TILT_FIVE_SDK)/include
#@END-C_SRC-DEFAULT-RULES@

APP=tiltfive
CFLAGS+=-std=c99 -Wall -g # -O3
LDFLAGS+=-shared -lTiltFiveNative -L $(TILT_FIVE_SDK)/lib/linux/x86_64

OBJS = tiltfive_nif.o
LIBS = 

all: ../priv/$(APP)_nif.so

../priv/$(APP)_nif.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o ../priv/$(APP)_nif.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
