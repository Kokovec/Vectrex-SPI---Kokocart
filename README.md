# Vectrex-SPI---Kokocart
A Synchronous Serial Interface for the Vectrex

These files contain the source code and Gerber files required to produce the Vectrex SPI Adapter that appeared in Vectorbolt issue #9 (http://geovector.tripod.com/_sgg/m3_1.htm).   
 
Within this repo you'll find:
* PSOC 3 Creator project (microcontroller code)
* VIDE project (Vectrex code)
* C64Studio project (C64 code)
* Gerber files (PCB)
* Pycharm project (Python ROM loader app)
* Various project docs (pictures, signal captures, etc) 
A video of the adapter in action can be found here: 
 
Procedure to load a ROM (up to 64K) onto the Adapter: 
* Plug Adapter into the Vectrex
* Connect USB cable from PC to Adapter
* Turn on Vectrex
* Run the Python app
* Within Python app:
  * Choose COM port
  * Choose ROM file 

The app will then transfer the ROM over to the Vectrex. 
To load a new ROM file without turning off the Vectrex you can do one of two things: 
* Hit the reset button on the app UI
* Press and hold the the Vectrex reset button for 2 seconds. In both cases the Vectrex will reboot to Minestorm but resetting the Vectrex (again) will kick off the loaded ROM.  

To load a new ROM file after the Vectrex has been turned off while the USB cable is still connected to the Adapter:
* Reset the Adapter by unplugging and re-plugging the USB connector 
* Restart the PC app and refer back to the original ROM loading procedure above 

This repo also include Vectrex and C64 programs that demonstrate how the SPI Adapter works. The Vectrex can control the movements of a ship on the C64 and the C64 can control the movements of a ship on the Vectrex. For this to work you will need to connect a cable between the Adpater and the C64 User Port. A pinout drawing can be found in the Project Docs folder.  
To run the demo:
* Turn on Vectrex and follow procedure to load Vectrex ROM file
* The screen will show "WAITING".
* Turn on the C64 and load the disk file
* Type in SYS 49152 and hit the return key  

Now you should be able to move the Vectrex and C64 joysticks around to control the ships on screen.  


