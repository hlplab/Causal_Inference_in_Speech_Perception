Note that custom qualifications that are meant for production (final experiment on MTurk) cannot be included in the YAML file for sandboxing. This and perhaps the number of assignments should be the only difference between the sandbox and production YAML files.

To upload a HIT to the sandbox, for example, go into the mturkutils/boto3/ folder and enter:

python3 loadHIT.boto3.py -s -c ../../../../experiments/YAML\ files\ for\ MTurk/Experiment-A-URLPARAMS-sandbox.yaml

You can add -p profile_name to specify a specific profile (e.g., I have my default personal profile and a lab profile). For details, see the tutorial on Setting up Mechanical Turk.

# Offline vs. online pseudo-randomization

We considered two approaches to randomization:

 1. making pseudo-randomized lists offline and then selecting between these prepared lists (and their reversed order).
 2. making pseudo-randomized lists online (within the Javascript) for each participant.

 LJ18 followed the first approach. Most other labs follow the second approach.

 ## Example of YAML file for offline pseudo-randomization

 With offline randomization and two prepared lists and their reverse oder (for exposure) plus a single test order, we would need 64 lists in total:

 question:
   url: https://www.hlp.rochester.edu/mturk/fjaeger/Experiment-A.html?{params}
   height: 750
   input:
   # 64 subjects per between-subject design condition; 256 subjects total
   # Condition code encodes the URL parameters in the following order:
   # condition: M[OUTH], H[AND]
   # label: S, SH
   # list_num: 1, 2
   # reverse: Y, N
   # respKeyExp: 0 ['X':'word', 'M':'non-word'], 1 ['X':'non-word', 'M':'word']
   # respKeyTest: 0 ['X':'S', 'M':'SH'], 1 ['X':'SH', 'M':'S']
   #
   # This results in a total of 2 x 2 design and 2 x 2 x 2 x 2 nuisance factors to balance
   # i.e, 4 x 16 = 64 different lists. We want 4 participants in each of these lists.
 - params: condition=M&label=S&list_num=1&reverse=N&respKeyExp=0&respKeyTest=0
 - params: condition=M&label=S&list_num=1&reverse=N&respKeyExp=0&respKeyTest=1
 - params: condition=M&label=S&list_num=1&reverse=N&respKeyExp=1&respKeyTest=0
 - params: condition=M&label=S&list_num=1&reverse=N&respKeyExp=1&respKeyTest=1
 - params: condition=M&label=S&list_num=1&reverse=Y&respKeyExp=0&respKeyTest=0
 - params: condition=M&label=S&list_num=1&reverse=Y&respKeyExp=0&respKeyTest=1
 - params: condition=M&label=S&list_num=1&reverse=Y&respKeyExp=1&respKeyTest=0
 - params: condition=M&label=S&list_num=1&reverse=Y&respKeyExp=1&respKeyTest=1
 - params: condition=M&label=S&list_num=2&reverse=N&respKeyExp=0&respKeyTest=0
 - params: condition=M&label=S&list_num=2&reverse=N&respKeyExp=0&respKeyTest=1
 - params: condition=M&label=S&list_num=2&reverse=N&respKeyExp=1&respKeyTest=0
 - params: condition=M&label=S&list_num=2&reverse=N&respKeyExp=1&respKeyTest=1
 - params: condition=M&label=S&list_num=2&reverse=Y&respKeyExp=0&respKeyTest=0
 - params: condition=M&label=S&list_num=2&reverse=Y&respKeyExp=0&respKeyTest=1
 - params: condition=M&label=S&list_num=2&reverse=Y&respKeyExp=1&respKeyTest=0
 - params: condition=M&label=S&list_num=2&reverse=Y&respKeyExp=1&respKeyTest=1
 - params: condition=M&label=SH&list_num=1&reverse=N&respKeyExp=0&respKeyTest=0
 - params: condition=M&label=SH&list_num=1&reverse=N&respKeyExp=0&respKeyTest=1
 - params: condition=M&label=SH&list_num=1&reverse=N&respKeyExp=1&respKeyTest=0
 - params: condition=M&label=SH&list_num=1&reverse=N&respKeyExp=1&respKeyTest=1
 - params: condition=M&label=SH&list_num=1&reverse=Y&respKeyExp=0&respKeyTest=0
 - params: condition=M&label=SH&list_num=1&reverse=Y&respKeyExp=0&respKeyTest=1
 - params: condition=M&label=SH&list_num=1&reverse=Y&respKeyExp=1&respKeyTest=0
 - params: condition=M&label=SH&list_num=1&reverse=Y&respKeyExp=1&respKeyTest=1
 - params: condition=M&label=SH&list_num=2&reverse=N&respKeyExp=0&respKeyTest=0
 - params: condition=M&label=SH&list_num=2&reverse=N&respKeyExp=0&respKeyTest=1
 - params: condition=M&label=SH&list_num=2&reverse=N&respKeyExp=1&respKeyTest=0
 - params: condition=M&label=SH&list_num=2&reverse=N&respKeyExp=1&respKeyTest=1
 - params: condition=M&label=SH&list_num=2&reverse=Y&respKeyExp=0&respKeyTest=0
 - params: condition=M&label=SH&list_num=2&reverse=Y&respKeyExp=0&respKeyTest=1
 - params: condition=M&label=SH&list_num=2&reverse=Y&respKeyExp=1&respKeyTest=0
 - params: condition=M&label=SH&list_num=2&reverse=Y&respKeyExp=1&respKeyTest=1
 - params: condition=H&label=S&list_num=1&reverse=N&respKeyExp=0&respKeyTest=0
 - params: condition=H&label=S&list_num=1&reverse=N&respKeyExp=0&respKeyTest=1
 - params: condition=H&label=S&list_num=1&reverse=N&respKeyExp=1&respKeyTest=0
 - params: condition=H&label=S&list_num=1&reverse=N&respKeyExp=1&respKeyTest=1
 - params: condition=H&label=S&list_num=1&reverse=Y&respKeyExp=0&respKeyTest=0
 - params: condition=H&label=S&list_num=1&reverse=Y&respKeyExp=0&respKeyTest=1
 - params: condition=H&label=S&list_num=1&reverse=Y&respKeyExp=1&respKeyTest=0
 - params: condition=H&label=S&list_num=1&reverse=Y&respKeyExp=1&respKeyTest=1
 - params: condition=H&label=S&list_num=2&reverse=N&respKeyExp=0&respKeyTest=0
 - params: condition=H&label=S&list_num=2&reverse=N&respKeyExp=0&respKeyTest=1
 - params: condition=H&label=S&list_num=2&reverse=N&respKeyExp=1&respKeyTest=0
 - params: condition=H&label=S&list_num=2&reverse=N&respKeyExp=1&respKeyTest=1
 - params: condition=H&label=S&list_num=2&reverse=Y&respKeyExp=0&respKeyTest=0
 - params: condition=H&label=S&list_num=2&reverse=Y&respKeyExp=0&respKeyTest=1
 - params: condition=H&label=S&list_num=2&reverse=Y&respKeyExp=1&respKeyTest=0
 - params: condition=H&label=S&list_num=2&reverse=Y&respKeyExp=1&respKeyTest=1
 - params: condition=H&label=SH&list_num=1&reverse=N&respKeyExp=0&respKeyTest=0
 - params: condition=H&label=SH&list_num=1&reverse=N&respKeyExp=0&respKeyTest=1
 - params: condition=H&label=SH&list_num=1&reverse=N&respKeyExp=1&respKeyTest=0
 - params: condition=H&label=SH&list_num=1&reverse=N&respKeyExp=1&respKeyTest=1
 - params: condition=H&label=SH&list_num=1&reverse=Y&respKeyExp=0&respKeyTest=0
 - params: condition=H&label=SH&list_num=1&reverse=Y&respKeyExp=0&respKeyTest=1
 - params: condition=H&label=SH&list_num=1&reverse=Y&respKeyExp=1&respKeyTest=0
 - params: condition=H&label=SH&list_num=1&reverse=Y&respKeyExp=1&respKeyTest=1
 - params: condition=H&label=SH&list_num=2&reverse=N&respKeyExp=0&respKeyTest=0
 - params: condition=H&label=SH&list_num=2&reverse=N&respKeyExp=0&respKeyTest=1
 - params: condition=H&label=SH&list_num=2&reverse=N&respKeyExp=1&respKeyTest=0
 - params: condition=H&label=SH&list_num=2&reverse=N&respKeyExp=1&respKeyTest=1
 - params: condition=H&label=SH&list_num=2&reverse=Y&respKeyExp=0&respKeyTest=0
 - params: condition=H&label=SH&list_num=2&reverse=Y&respKeyExp=0&respKeyTest=1
 - params: condition=H&label=SH&list_num=2&reverse=Y&respKeyExp=1&respKeyTest=0
 - params: condition=H&label=SH&list_num=2&reverse=Y&respKeyExp=1&respKeyTest=1
