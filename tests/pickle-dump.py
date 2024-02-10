#! /usr/bin/env python
import argparse
import pickle
import pickletools

def run(args):
  expr = eval(args.expression)
  protocol = int(args.protocol)
  if args.output:
    with open(args.output, 'wb') as fh:
      pickle.dump(expr, fh, protocol=protocol)
  else:
      print(repr(pickle.dumps(expr, protocol=protocol)))

  if not args.output and args.disassembly:
    d = pickle.dumps(expr, protocol=protocol)
    pickletools.dis(d)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="Eval a string and dump it using pickle."
    )
    parser.add_argument('expression', metavar='EXPR',
        help="Python expression to dump.")
    parser.add_argument('--disassembly', action='store_true',
        help="Show a disassembly of dumped data.")
    parser.add_argument('--output', metavar='FILE',
        help="Dump to the given file.")
    parser.add_argument('--protocol', default=2,
        help="Dump to the given file.")
    parser.set_defaults(run=run)

    args = parser.parse_args()
    args.run(args)
