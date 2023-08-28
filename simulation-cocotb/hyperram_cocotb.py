#https://docs.cocotb.org/en/v1.5.0/testbench_tools.html#driving-buses
#https://docs.cocotb.org/en/v1.1/library_reference.html?highlight=avalon#avalon
#https://github.com/cocotb/cocotb-bus/blob/master/src/cocotb_bus/drivers/avalon.py
#https://docs.cocotb.org/en/v1.1/hal_cosimulation.html?highlight=avalon

#https://github.com/cocotb/cocotb-bus/blob/master/examples/endian_swapper/tests/test_endian_swapper.py

import os
import random
import sys
from pathlib import Path
from enum import Enum

import cocotb
from cocotb.clock import Clock
#from cocotb.runner import get_runner
from cocotb.triggers import Timer, RisingEdge, FallingEdge
from cocotb.types import LogicArray

#This is needed to create phase shifted clock
#https://github.com/cocotb/cocotb/issues/1158

@cocotb.coroutine
async def my_custom_clock():
    await Timer(my_initial_delay)
    while True:
        sig <= 1
        await Timer(my_high_delay)
        sig <= 0
        await Timer(low)