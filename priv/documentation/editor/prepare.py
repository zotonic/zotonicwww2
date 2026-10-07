#!/usr/bin/env python3
"""Validate this guide; --render also produces an offline preview."""
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(ROOT.parent))
from prepare_common import main

if __name__ == "__main__":
    main(ROOT)
