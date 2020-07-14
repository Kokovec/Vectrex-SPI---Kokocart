# Vectrex-SPI---Kokocart
A Synchronous Serial Interface for the Vectrex

These files contain the source code and Gerber files required to produce the Vectrex SPI adapter that appeared in Vectorbolt issue #9 (http://geovector.tripod.com/_sgg/m3_1.htm).   
 
Within this repo you'll find:
* PSOC 3 Creator project (microcontroller code)
* VIDE project (Vectrex code)
* C64Studio project (C64 code)
* Gerber files (PCB)
* Pycharm project (Python ROM loadder app)
* Various project docs (pictures, signal captures, etc) 
A video of the adapter in action can be found here: 
 
Procedure to load a ROM (up to 64K) onto the Adapter: 
* Plug adapter into the Vectrex
* Connect USB cable from PC to Adapter
* Turn on Vectrex
* Run the Python app
* Within Python app:
  * Choose com port
  * Choose ROM file 

The app will then transfer the ROM over to the Vectrex.
