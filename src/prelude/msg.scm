;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(c-declare #<<c-declare-end

#include "nn.h"
#include "pipeline.h"

int c_msg_socket () { return nn_socket (AF_SP, NN_PULL); }

c-declare-end
)

(define msg_socket (c-lambda () int "c_msg_socket"))
