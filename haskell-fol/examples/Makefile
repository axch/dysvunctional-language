### ----------------------------------------------------------------------
### Copyright 2013 Alexey Radul.
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

FOLJS = $(shell which fol2js)

celestial.js: celestial-no-fractions.fol
ifneq (,$(strip $(FOLJS)))
	fol2js < celestial-no-fractions.fol > celestial.js
else
	@echo No 'fol2js' executable found.
	@echo Run 'cabal configure && cabal build && cabal install' in the FOL root directory first.
endif
