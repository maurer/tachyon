#!/bin/sh
sudo sysctl kernel.vsyscall64=0
sudo sysctl kernel.randomize_va_space=0
