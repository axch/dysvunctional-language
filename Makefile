### ----------------------------------------------------------------------
### Copyright 2010-2011 National University of Ireland; 2013 Alexey Radul.
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

clean:
	@find . -regextype posix-extended -regex ".*\.(bci|bin|com|ext)$$" -delete

test: test-fol test-vl test-dvl

test-fol: ; $(MAKE) -C fol test
test-vl:  ; $(MAKE) -C vl  test
test-dvl: ; $(MAKE) -C dvl test

.PHONY: clean test test-fol test-vl test-dvl
