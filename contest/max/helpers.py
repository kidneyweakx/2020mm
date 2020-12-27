#!/usr/bin/env python3

from time import time as _time
from datetime import datetime

def get_current_timestamp():
    # print(datetime.now().timestamp, int(round(_time() * 1000))
    return int(round(_time() * 1000))
