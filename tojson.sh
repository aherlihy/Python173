#python

alias python3='/course/cs173/python/Python-3.2.3/python'
#usage: echo <test_expr> | test_py 
alias test_py='racket python-main.rkt --python-path /course/cs173/python/Python-3.2.3/python --interp-py'
#cd into python directory
alias 173py='cd ~/Python173'
alias 173test_all='racket python-main.rkt --python-path /course/cs173/python/Python-3.2.3/python --test python-reference'
#usage: cat <test_file> | 173beautifier
alias 173b='python python-parser.py | python jsbeautifier.py -i'