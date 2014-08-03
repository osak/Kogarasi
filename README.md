# Kogarasi
A comment server written in Haskell.

# Setup

1. Setting up database
    
        $ sudo -u postgres createdb -O kogarasi kogarasi

2. Write database settings in DBSetting.hs
3. Build

        $ cabal install persistent persistent-postgresql scotty
        $ ghc Migrator
        $ ghc ScottyMain

4. Prepare DB

        $ ./Migrator

5. Run

        $ sudo -u kogarasi ./ScottyMain
