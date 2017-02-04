(deftemplate planet
    (slot planetName)
    (slot mass (type FLOAT))
    (slot radius (type FLOAT))
    (slot tsurf (type FLOAT))
    (slot teq (type FLOAT))
    (slot period (type INTEGER))
    (slot distanceFromParentStar (type FLOAT))
)

;Information about the parent Star
(deftemplate parentStar 
    (slot planetName)
    (slot starName)
    (slot stellarEffTemperature (type FLOAT))
    (slot stellarRadius (type FLOAT))
    (slot stellarLuminosity (default 0) (type FLOAT))
)

;Derived / Calculated Characteristics
(deftemplate planetCharacteristics
    (slot planetName)
    (slot HZD (type FLOAT)(default 0))
    (slot terrainType (default NONE))
    (slot terrainTypeDesc (default NONE))
    (slot ri (type FLOAT)(default 0))
    (slot ro (type FLOAT)(default 0))
    (slot TPHC (default NONE))
    (slot TPHCDesc (default NONE))
    (slot interiorESI(type FLOAT)(default 0))
    (slot surfaceESI(type FLOAT)(default 0))
    (slot globalESI(type FLOAT)(default 0))
    (slot gravity (type FLOAT)(default 0))
    (slot escapeVelocity (type FLOAT)(default 0))
    (slot density (type FLOAT)(default 0))
)

;Planet Habitabilty Judgements
(deftemplate currentJudgementState
    (slot planetName)
    (slot isInteriorESIGood (default NONE))
    (slot InteriorESIDesc (default NONE))
    (slot isSurfaceESIGood (default NONE))
    (slot surfaceESIDesc (default NONE))
    (slot isGlobalESIGood (default NONE))
    (slot globalESIDesc(default NONE))
    (slot isHZDGood (default NONE))
    (slot HZDDesc(default NONE))
    (slot isTempGood (default NONE))
    (slot resultsFilled(default FALSE))
    (slot isPotentiallyHabitable (default NONE))
    (slot beginAnalysis (default NONE))
    (slot analysisComplete (default NONE))
    (slot judgementComplete (default NONE))
)

