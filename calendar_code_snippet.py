# Enter your code here. Read input from STDIN. Print output to STDOUT
import calendar
from datetime import datetime
import sys

for input_date in sys.stdin:
    actual_date = datetime.strptime(input_date, '%m %d %Y')
    print calendar.day_name[actual_date.weekday()].upper()