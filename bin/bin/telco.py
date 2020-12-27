#! /usr/bin/env python3

import sys
import argparse
import phonenumbers
import phonenumbers.carrier


def parse_arguments(argv):
    parser = argparse.ArgumentParser()

    parser.add_argument("phone_number", default=None, help="The number")

    args = parser.parse_args(argv)
    return args


def main(argv):
    args = parse_arguments(argv)

    p = phonenumbers.parse(args.phone_number)
    print("Country:", phonenumbers.region_code_for_country_code(p.country_code).lower())
    print("Telco:", phonenumbers.carrier.name_for_valid_number(p, "en"))
    print("valid:", phonenumbers.is_valid_number(p))

    return 0


# If this script is run from a shell then run main() and return the result.
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
