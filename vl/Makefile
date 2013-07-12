### ----------------------------------------------------------------------
### Copyright 2010-2011 National University of Ireland, 2013 Alexey Radul.
### ----------------------------------------------------------------------
### This file is part of DysVunctional Language.
### 
### DysVunctional Language is free software; you can redistribute it and/or modify
### it under the terms of the GNU Affero General Public License as
### published by the Free Software Foundation, either version 3 of the
###  License, or (at your option) any later version.
### 
### DysVunctional Language is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
### 
### You should have received a copy of the GNU Affero General Public License
### along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
### ----------------------------------------------------------------------

ARCH = $(shell uname -m)

ifeq ($(ARCH), x86_64)
  HEAP = 100000
else
  HEAP = 6000
endif

test:
	mit-scheme --compiler --heap $(HEAP) --stack 2000 --batch-mode --no-init-file \
	  --eval '(set! load/suppress-loading-message? #t)' \
	  --eval '(begin (load "load") (load "test/load") (run-tests-and-exit))'

FILES = examples/small.vl abstract-eval.scm code-generator.scm macro.scm eval.scm primitives.scm syntax.scm data.scm env.scm analysis.scm abstract-values.scm nomenclature.scm load.scm

workbook: $(FILES)
	enscript -M a4 -fCourier-Bold12 -o workbook-a4.ps --file-align=2 README  --color --highlight $(FILES)
	enscript -M letter -fCourier-Bold12 -o workbook-letter.ps --file-align=2 README  --color --highlight $(FILES)

.PHONY: workbook test
