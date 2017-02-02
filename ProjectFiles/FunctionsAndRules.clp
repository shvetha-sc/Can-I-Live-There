;Functions
;A decorator function for messages
(deffunction decorator(?message)
    (printout t crlf)
    (printout t "---------------------------" ?message "-----------------------------------" crlf)
)
;Function for Calculating the Habitable Zone Distance
(deffunction calculateHZD (?stellarEffTemperature ?stellarLuminosity ?distanceFromParentStar ?p)
    (bind ?ri (* (- 0.72 (+ (* 0.000027619 (- ?stellarEffTemperature 5700)) (* 0.0000000038095 (** (- ?stellarEffTemperature 5700 ) 2))) ) (sqrt ?stellarLuminosity)))
    (bind ?ro (* (- 1.77 (+ (* 0.00013786 (- ?stellarEffTemperature 5700)) (* 0.0000000014286 (** (- ?stellarEffTemperature 5700 ) 2) )) ) (sqrt ?stellarLuminosity)   )   )
    (modify ?p (ri ?ri))
    (modify ?p (ro ?ro))
    (bind ?hzd (/ (- (* 2 ?distanceFromParentStar) (+ ?ro ?ri)) (- ?ro ?ri)))
    (return ?hzd)
)

;Welcome information for user
(deffunction displayWelcomeInfo()
    (printout t crlf)
    (decorator "CAN WE LIVE HERE")
    (printout t crlf)
    (printout t "Welcome to 'CAN WE LIVE HERE', a potentially habitable exoplanet classification system." crlf)
    (printout t "Based on your information we would let you know as to whether the planet you chose would be your new home!" crlf)
    (printout t crlf)
)

;Function to calculate the Global ESI
(deffunction globalESICalc(?radius ?density ?escapeVelocity ?temperature)
    (bind ?temp1 (** (- 1 (abs (/ (- ?radius 1)(+ ?radius 1)))) 0.57))
    (bind ?temp2 (** (- 1 (abs (/ (- ?density 1)(+ ?density 1)))) 1.07))
    (bind ?temp3 (** (- 1 (abs (/ (- ?escapeVelocity 1)(+ ?escapeVelocity 1)))) 0.70))
    (bind ?temp4 (** (- 1 (abs (/ (- ?temperature 288)(+ ?temperature 288)))) 5.58))
    (return (** (* ?temp1 ?temp2 ?temp3 ?temp4) 0.25))
)

;Function to Display the Planet Information
(deffunction displayPlanetInfo(?planetInfo)
    (decorator "Entered Exo-Planet Information")
    (printout t  crlf)
    (printout t "Planet Name : " ?planetInfo.planetName crlf)
    (printout t "Planet Mass : " ?planetInfo.mass " EU" crlf)
    (printout t "Planet Radius : " ?planetInfo.radius " EU" crlf)
    (printout t "Surface Temperature : " ?planetInfo.tsurf " K" crlf)
    (printout t "Equlibrium Temperature : " ?planetInfo.teq " K" crlf)
    (printout t "Orbital period : " ?planetInfo.period " days" crlf)
)

;Function to Display the Parent Star Information
(deffunction displayParentStarInfo(?parentInfo)
    (decorator "Entered Exo-Planet's Parent Star Information")
    (printout t  crlf)
    (printout t "Parent Star Name : " ?parentInfo.starName crlf)
    (printout t "Parent Star Radius : " ?parentInfo.stellarRadius " SU" crlf)
    (printout t "Parent Star Effective Temperature : " ?parentInfo.stellarEffTemperature " K" crlf)
)

;Function to calculate Gravity
(deffunction calculateGravity(?mass ?radius)
    (bind ?temp (/ ?mass (** ?radius 2)))
    (return ?temp)
)

;Function to calculate Escape velocity
(deffunction calculateEscapeVelocity(?mass ?radius)
    (bind ?temp2 (/ ?mass ?radius))
    (bind ?temp (sqrt ?temp2))
    (return ?temp)
)

;Function to calculate Density
(deffunction calculateDensity(?mass ?radius)
    (bind ?temp (/ ?mass (** ?radius 3)))
    (return ?temp)
)

;Function to calculate Stellar Luminosity
(deffunction calculateStellarLuminosity (?stellarRadius ?stellarTemperature)
    (bind ?temp (* (** ?stellarRadius 2)(** (/ ?stellarTemperature 5780) 4)))
    (return ?temp)
)

;Function to calculate interior ESI
(deffunction interiorESICalc(?radius ?density)
    (bind ?temp1 (** (- 1 (abs (/ (- ?radius 1)(+ ?radius 1)))) 0.57))
    (bind ?temp2 (** (- 1 (abs (/ (- ?density 1)(+ ?density 1)))) 1.07))
    (return (sqrt (* ?temp1 ?temp2)))
)

;Function to calculate Surface ESI
(deffunction surfaceESICalc(?escapeVelocity ?temperature)
    (bind ?temp1 (** (- 1 (abs (/ (- ?escapeVelocity 1)(+ ?escapeVelocity 1)))) 0.70))
    (bind ?temp2 (** (- 1 (abs (/ (- ?temperature 288)(+ ?temperature 288)))) 5.58))
    (return (sqrt (* ?temp1 ?temp2)))
)


;Rules-----------------------------------
;Rule 1 : Display Entered ExoPlanet and Parent Star Information
(defrule displayPlanetInfo
    ?p <- (planet (planetName ?planetName))
    ?p1 <-(parentStar {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
   	(if (and (= ?p3.resultsFilled TRUE) (<> ?p3.beginAnalysis TRUE))then
    (displayWelcomeInfo)
 	(displayPlanetInfo ?p)  
    (displayParentStarInfo ?p1)
    (modify ?p3 (beginAnalysis TRUE))
    (printout t crlf))
)

;Rule 2 : Generate Additional Facts for further analysis
(defrule calculateAdditionInformation
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(parentStar {planetName == p.planetName})
    ?p3 <-(planetCharacteristics {planetName == p.planetName})
    ?p4 <-(currentJudgementState {planetName == p.planetName})
    =>
    ;Gravity
    (modify ?p3 (gravity (calculateGravity ?p.mass ?p.radius)))
    ;Escape Velocity
    (bind ?escapeVelocityValue (calculateEscapeVelocity ?p.mass ?p.radius))
    (modify ?p3 (escapeVelocity ?escapeVelocityValue))
    ;Density
   	(modify ?p3 (density (calculateDensity ?p.mass ?p.radius)))
    ;Calculate Stellar Values
    (modify ?p2 (stellarLuminosity (calculateStellarLuminosity ?p2.stellarRadius ?p2.stellarEffTemperature)))
    ;ESI Values
    (modify ?p3 (interiorESI (interiorESICalc ?p.radius ?p3.density)))
    (modify ?p3 (surfaceESI (surfaceESICalc ?p3.escapeVelocity ?p.tsurf)))
    (modify ?p3 (globalESI (globalESICalc ?p.radius ?p3.density ?p3.escapeVelocity ?p.tsurf)))
    ;HZD Values
  	(modify ?p3 (HZD (calculateHZD ?p2.stellarEffTemperature ?p2.stellarLuminosity ?p.distanceFromParentStar ?p3)))
    (modify ?p4 (resultsFilled TRUE))
)

;Planet Mass Based Determination
;Rule 3 : Planet Type : Jovian
(defrule Jovian
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    =>
    (if (and (> ?p.mass 50)(< ?p.mass 5000)(> ?p.radius 3.5 )(< ?p.radius 27) (= ?p2.terrainType NONE) )then
    	(modify ?p2 (terrainType Jovian))
        (modify ?p2 (terrainTypeDesc "Based on its mass, the exoplanet is classified as a Jovian.These can have superdense atmospheres"))     
     )
)

;Rule 4 : Planet Type : Terran
(defrule Terran
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    =>
    (if (and (> ?p.mass 0.5)(< ?p.mass 2)(> ?p.radius 0.8)(< ?p.radius 1.9)(= ?p2.terrainType NONE))then
    	(modify ?p2 (terrainType Terran))
        (modify ?p2 (terrainTypeDesc "Based on its mass, the exoplanet is classified as a Terran.These are able to hold a significant atmosphere with liquid water."))        
     )
)

;Rule 5 : Planet Type : Super Terran
(defrule SuperTerran
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    =>
    (if (and (> ?p.mass 2)(< ?p.mass 10)(> ?p.radius 1.3)(< ?p.radius 3.3)(= ?p2.terrainType NONE))then
    	(modify ?p2 (terrainType SuperTerran))
        (modify ?p2 (terrainTypeDesc "Based on its mass, the exoplanet is classified as a SuperTerran.These are able to hold dense atmospheres with liquid water"))             
     )
)

;Rule 6 : Planet Type : Neptunian
(defrule Neptunian
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    =>
   (if (and (> ?p.mass 10)(< ?p.mass 50)(> ?p.radius 2.1 )(< ?p.radius 5.7)(= ?p2.terrainType NONE))then
    	(modify ?p2 (terrainType Neptunian))
        (modify ?p2 (terrainTypeDesc "Based on its mass, the exoplanet is classified as a Neptunain.These can have dense atmospheres."))                  
     )
)

(defrule undeterminedTerrainType
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
   (if (or (< ?p.mass 0.5)(> ?p.mass 5000)(> ?p.radius 27 )(< ?p.radius 0.8)(= ?p2.terrainType NONE))then
        (modify ?p2 (terrainType "CANNOT BE DETERMINED"))
        (modify ?p2 (terrainTypeDesc "Based on its mass, the exoplanet type cannot be determined."))                  
   
    )
    )

;ESI Judgement Rules
;Rule 7 : Interior ESI Determination (+)
(defrule goodInteriorESI
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p2.interiorESI 0.7)(= ?p3.isInteriorESIGood NONE)(= ?p3.beginAnalysis TRUE)) then
        (bind ?temp "The above interior ESI shows that the planet has a highly rocky interior")
        (modify ?p3 (InteriorESIDesc ?temp))
        (modify ?p3 (isInteriorESIGood TRUE))
        )
    )

;Rule 8 : Interior ESI Determination (-)
(defrule badInteriorESI
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (< ?p2.interiorESI 0.7) (= ?p3.isInteriorESIGood NONE)(= ?p3.beginAnalysis TRUE)) then       
        (bind ?temp "The above interior ESI shows that the planet does not have a highly rocky interior")
        (modify ?p3 (InteriorESIDesc ?temp))
        (modify ?p3 (isInteriorESIGood FALSE))
        )
    )

;Rule 9 : Surface ESI Determination (+)
(defrule goodSurfaceInteriorESI
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p2.surfaceESI 0.7)(= ?p3.isSurfaceESIGood NONE)(= ?p3.beginAnalysis TRUE)) then
        (bind ?temp "The above surface ESI shows that the planet has a temperate surface.")
        (modify ?p3 (surfaceESIDesc ?temp))
        (modify ?p3 (isSurfaceESIGood TRUE))
        )
    )

;Rule 10 : Surface ESI Determination (-)
(defrule badSurfaceESI
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (< ?p2.surfaceESI 0.7) (= ?p3.isSurfaceESIGood NONE)(= ?p3.beginAnalysis TRUE)) then        
        (bind ?temp "The above surface ESI shows that the planet does not have a temperate surface.")
        (modify ?p3 (surfaceESIDesc ?temp))
        (modify ?p3 (isSurfaceESIGood FALSE))
        )
)

;Rule 11 :  Global  ESI Determination (+)
(defrule goodGlobalESI
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p2.globalESI 0.7)(= ?p3.isGlobalESIGood NONE) (<> ?p3.isSurfaceESIGood NONE) (<> ?p3.isInteriorESIGood NONE)(= ?p3.beginAnalysis TRUE)) then        
        (bind ?temp "The above global ESI is considered good")
        (modify ?p3 (globalESIDesc ?temp))
        (modify ?p3 (isGlobalESIGood TRUE))
        )
    )

;Rule 12 : Global ESI Determination (-)
(defrule badGlobalESI
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (< ?p2.globalESI 0.7) (= ?p3.isGlobalESIGood NONE) (<> ?p3.isSurfaceESIGood NONE) (<> ?p3.isInteriorESIGood NONE)(= ?p3.beginAnalysis TRUE)) then             
        (bind ?temp "The above global ESI is not considered good")
        (modify ?p3 (globalESIDesc ?temp))
        (modify ?p3 (isGlobalESIGood FALSE))
        )
)


;Habitable Zone Distance Determination Rules
;Rule 13 : HZD Determination (+)
(defrule goodHZD
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p2.HZD -1.1) (< ?p2.HZD 1.1)(= ?p3.isHZDGood NONE)(= ?p3.beginAnalysis TRUE)) then        
 
        (bind ?temp "This tells us that the planet lies within the habitable zone")
        (modify ?p3 (HZDDesc ?temp))
        (modify ?p3 (isHZDGood TRUE))
        )
    )

;Rule 14 : HZD Determination (-)
(defrule badHZD
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (< ?p2.HZD -1.1) (= ?p3.isHZDGood NONE)(= ?p3.beginAnalysis TRUE)) then        
        (bind ?temp "This tells us that the planet does not lie within the habitable zone")
        (modify ?p3 (HZDDesc ?temp))
        (modify ?p3 (isHZDGood FALSE))
        )
     (if (and (> ?p2.HZD 1.1) (= ?p3.isHZDGood NONE)(= ?p3.beginAnalysis TRUE)) then        
        (bind ?temp "This tells us that the planet does not lie within the habitable zone")
        (modify ?p3 (HZDDesc ?temp))
        (modify ?p3 (isHZDGood FALSE))
        )
)

;Rules : Type Determination based on Planet Surface Temperature
;Rule 15 : Planet Type : Class M
(defrule CLASS_M
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p.tsurf 273.15) (< ?p.tsurf 323.15)(= ?p3.isTempGood NONE)(= ?p3.beginAnalysis TRUE)) then
       	(modify ?p2 (TPHCDesc "It belongs to Class M (MesoPlanet) : This category belongs to earth like planets with temperatures ranging from 0 degree C to 50 degree C." crlf))
        (modify ?p2 (TPHC "Class M"))
        (modify ?p3 (isTempGood TRUE))
        )
    )

;Rule 16 : Planet Type : Class P
(defrule CLASS_P
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p.tsurf 223.15) (< ?p.tsurf 273.16)(= ?p3.isTempGood NONE)(= ?p3.beginAnalysis TRUE)) then
        (modify ?p2 (TPHCDesc "It belongs to Class P (Psychroplanet) : This tells us that is planet is cold with temperatures ranging from -50 degree C to 0 degree C." crlf))
   		(modify ?p2 (TPHC "Class P"))
        (modify ?p3 (isTempGood FALSE))
        )
)

;Rule 17 : Planet Type : Class hP
(defrule Class_hP
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p.tsurf 223.15)(< ?p.tsurf 173.15)(= ?p3.isTempGood NONE)(= ?p3.beginAnalysis TRUE)) then
        (modify ?p2 (TPHCDesc "It belongs to Class hP (Hypopsychroplanet) : This tells us that is planet is extemely cold with temperatures lower than -50 degree C." crlf))		
        (modify ?p2 (TPHC "Class hP"))
        (modify ?p3 (isTempGood FALSE))
        )
)

;Rule 18 : Planet Type : Class T
(defrule Class_T
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p.tsurf 323.15) (< ?p.tsurf 373.15)(= ?p3.isTempGood NONE)(= ?p3.beginAnalysis TRUE)) then
       	(modify ?p2 (TPHCDesc "It belongs to Class T (Thermoplanet) : This tells us that is planet is hot with temperatures ranging from 0 degree C and 50 degree C." crlf))		
        (modify ?p2 (TPHC "Class T"))
        (modify ?p3 (isTempGood FALSE))
        )
)

;Rule 19 : Planet Type : Class hT
(defrule Class_hT
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(planetCharacteristics {planetName == p.planetName})
    ?p3 <-(currentJudgementState {planetName == p.planetName})
    =>
    (if (and (> ?p.tsurf 373.15)(= ?p3.isTempGood NONE)(= ?p3.beginAnalysis TRUE)) then
        (modify ?p2 (TPHCDesc "It belongs to Class hT (Hyperthermoplanet) : This tells us that is planet is extremely hot with temperatures above 100 degree C." crlf))		
        (modify ?p2 (TPHC "Class hT"))
        (modify ?p3 (isTempGood FALSE))
        )
)

;Rule 20 : This rule will be triggered when all the information in order to judge the planet type is complete
(defrule AllConditionsCheck
     ?p <-(planet (planetName ?planetName))
    ?p2 <-(currentJudgementState {planetName == p.planetName})
    ?p3 <-(planetCharacteristics {planetName == p.planetName})
    =>
     (if (and (<> ?p2.isInteriorESIGood NONE)(<> ?p2.isSurfaceESIGood NONE) (<> ?p2.isGlobalESIGood NONE) (<> ?p2.isHZDGood NONE) (<> ?p2.isTempGood NONE) (<> ?p3.terrainType NONE))then
    	(if (and (= ?p2.isInteriorESIGood TRUE)(= ?p2.isSurfaceESIGood TRUE) (= ?p2.isGlobalESIGood TRUE) (= ?p2.isHZDGood TRUE) (= ?p2.isTempGood TRUE) (<> ?p3.terrainType NONE))then
            (modify ?p2 (analysisComplete TRUE))
            (modify ?p2 (isPotentiallyHabitable TRUE))
        else
            (modify ?p2 (isPotentiallyHabitable FALSE))
            (modify ?p2 (analysisComplete TRUE))
        )
    )
)


;Rule 21 : This will display the formatted output to the user
(defrule DisplayOutputPostive
    ?p <-(planet (planetName ?planetName))
    ?p2 <-(currentJudgementState {planetName == p.planetName})
    ?p3 <-(planetCharacteristics {planetName == p.planetName})
    =>
    (if (and (= ?p2.resultsFilled TRUE)(= ?p2.analysisComplete TRUE) (<> ?p2.judgementComplete TRUE) )then
        (decorator "Further Analysis")
        (if (<> ?p3.terrainTypeDesc NONE)then
    	(decorator "Planet Mass Classification")    
        (printout t ?p3.terrainTypeDesc crlf))
        (decorator " Interior ESI (Earth Similarity Index)")
        (printout t "This ESI is based on the mean radius and bulk density of the exoplanet" crlf)
        (printout t "Interior ESI : " ?p3.interiorESI  crlf)
        (printout t ?p2.InteriorESIDesc crlf)
        (decorator " Surface ESI (Earth Similarity Index)")
        (printout t "This ESI is based on the escape velocity and surface temperature of the exoplanet" crlf)
        (printout t "Surface ESI : " ?p3.surfaceESI  crlf)
        (printout t ?p2.surfaceESIDesc crlf)
        (decorator " Global ESI (Earth Similarity Index)")
        (printout t "This ESI is based combination of values of the Interior and Surface ESI" crlf)
        (printout t "Global ESI : " ?p3.globalESI  crlf)
        (printout t ?p2.globalESIDesc crlf)
        (decorator "Habitable Zone Distance (HZD)")
        (printout t "This defined as the region around a star were a planet could maintain liquid water at their surface" crlf)
        (printout t "The HZD is  : "?p3.HZD crlf)
        (printout t ?p2.HZDDesc crlf)
        (decorator "Thermal Planetary Habitability Classification (T-PHC)")
        (printout t "This represents the potential habitable exoplanets based on their mean global surface temperature" crlf)
	   	(printout t ?p3.TPHCDesc)
       	(modify ?p2 (judgementComplete TRUE))
        
        )	
    )

;Rule 22 : This will also display the formatted output to the user



;Rule 23 : Final Judgment Positive
(defrule FinalJudgement
     ?p <-(planet (planetName ?planetName))
    ?p2 <-(currentJudgementState {planetName == p.planetName})
    ?p3 <-(planetCharacteristics {planetName == p.planetName})
    =>
    (if (and (= ?p2.resultsFilled TRUE)(= ?p2.analysisComplete TRUE) (<> ?p2.isPotentiallyHabitable NONE) )then
      (printout t crlf)
        (if (= ?p2.isPotentiallyHabitable TRUE)then
    	(decorator "Final Analysis")
        (printout t "From the Above Analysis, it seems that the planet can be considered potentially habitable." crlf)
        else
        (decorator "Final Analysis")
        (printout t "From the Above Analysis, it seems that the planet cannot be considered potentially habitable." crlf)
       )
  )
)


(run)
;(facts)	
(reset)
(clear)
(exit)


;The above information/rules are a part of an ongoing research and is subject to change as and when new accurate information is available.
