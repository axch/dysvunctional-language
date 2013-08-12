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

FONT1=Courier-Bold24

%.txt.ps: %.txt
	enscript -r -M letter --highlight --color -f$(FONT1) -o $@ $<

%.dvl.ps: %.dvl
	enscript -r -M letter --highlight=scheme --color -f$(FONT1) -o $@ $<

%.fol.ps: %.fol
	enscript -r -M letter --highlight=scheme --color -f$(FONT1) -o $@ $<

%.js.ps: %.js
	enscript -r -M letter --highlight --color -f$(FONT1) -o $@ $<

%.pdf: %.ps
	ps2pdf $< $@

presentation.pdf: mandel.js.pdf mandel.dvl.pdf cognoscenti.txt.pdf analyzed.fol.pdf credits.txt.pdf
	pdfjoin -o $@ $+
