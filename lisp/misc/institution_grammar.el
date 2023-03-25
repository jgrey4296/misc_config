;; ADICO
;; Attributes
;; Deontics
;; Aims
;; Conditions
;; Or Else
(let ((norm-change (tracerel-grammar
                    start "#norm_change#"
                    norm_change  "#attributeChange#" "#deonticChange#" "#sanctionChange#" "#iguChange#" "#sanctionEnforcementChange#" "#repeal#" "#ritualChange#" "#challengeIncumbentChange#" "#shallowChange#" "#prohibitionChange#" "#timeExtend#"
                    prelude  "If there is a rule"
                    timeExtend  "If a rule will sunset in the next #n# legislative turns, and it is beneficial to you, extend its effective time"
                    prohibitionChange "If an action or item that you #maybeInnate# favour is prohibited, remove that prohibition" "If an action or item that you #maybeInnate# dislike is allowed, add a prohibition to it"

                    shallowChange "If the institution's colours are not to your liking, change them" "If the institution's name is not to your liking, change it" "If you dislike the institution's clothing requirements, change them"

                    challengeIncumbentChange "If there is a challenger role you are allied with, promote it to be a new incumbent" "If there is an incumbent role you are feuding with, demote it to be a new challenger"
                    attributeChange  "#prelude# where the #roleType# must be #role#, change it to #role##maybeTime##maybeReason#"
                    deonticChange  "#prelude# where an actor #deontic# perform an action, change it so that they #deontic# perform the action#maybeTime#"
                    sanctionChange "#prelude# where transgression of the rule specifies #sanction#, change it to #sanction##maybeTime#",
                    sanctionEnforcementChange  "#prelude# where the transgression of a rule is enforced by #role#, change it to #role##maybeTime#"
                    iguChange  "If the IGU is structured as #iguType#, change it to #iguType##maybeTime#"
                    repeal  "If there #repealType#, repeal it#maybeTime#"
                    ritualChange  "If there is a ritual sequence that hinders a favoured group, make it easier by removing one of the ritual steps"
                    "If there is a ritual sequence that benefits a disliked group, make it harder by adding a ritual step"

                    repealType  "are multiple constitutive rules that designate individuals in your own role type, repeal one of them#maybeTime#"
                    "are regulative constraints on an action you enjoy, remove one#maybeTime#"

                    maybeInnate  "" "innately" "socially"
                    maybeTime  "" ", for the next #n# legistlative turns"
                    n "1" "2" "3" "4" "5" "6" "7" "8" "9"
                    maybeReason  "" " if it will #benefit# the #oldNew# role"
                    oldNew  "old" "new"
                    benefit  "benefit" "hinder"
                    roleType  "Actor" "Target" "Bystander" "Partner"
                    role  "A" "B" "C" "D" "E" "F" "G"
                    deontic  "must" "can" "can't" "should" "should not" "may" "shall" "shall not" "is permitted" "is not permitted"
                    sanction  "exile" "execution" "torture" "mockery" "whipping" "to be placed in the stocks" "hanging"
                    "social exile" "arrest" "dismemberment" "a fine" "civil forfeiture"

                    iguType  "Democracy" "Council" "Dictatorship" "Overseer" "Judges"
                    ))
      ;;----------------------------------------
      (institution (tracerel-grammar
                    ;;Inter-usage consistency layers:
                    ExpansionLayers "institutionColour" "incumbents" "challengers" "field"
                    ;;Main origin
                    start "#institution#"
                    ;;The Total Institution
                    institution  "An Institution: #institutionName#. <p> The preferred colour is #institutionColour#. <p> The Incumbents are #incumbents.s#. <p> The Challengers are #challengers.s#. <p> The Core Concepts are #concepts#. <p>The field is guided by the field rules #field#. <p> #clothingStandards#. <p> The field is overseen by #igu#, with #amnt# corruption.<p> #enforcement# <p>Advancement is achieved through #promotion#. <p>#stateRelation#. <p>Relations with other institutions: #externalLinks#. <p> The institution can suffer crises from #crises#."
                    ;;The IGU as an institution, without infinite recursion
                    iguLessInstitution  "#incumbents# #challengers# #field# #stateRelation# #externalLinks# #crises#"

                    ;;Consistency layer expansions:
                    institutionColour  "#colour#"
                    incumbents "#role# % #role# % #role#"
                    challengers  "#role# % #role# % #role#"
                    field  "#norm# #norm# and #norm#"

                    maybeInstitutionPrefix  "" "#institutionColour#"

                    institutionType  "Church" "Office" "Gang" "Group" "School" "Way" "Cult" "Path" "Control" "Prison" "Breeding" "Monarchy"
                    institutionTarget   "#incumbents#" "#challengers#" "#role.s#" "#promotion.capitalize#" "#concept#"
                    institutionName  "The #maybeInstitutionPrefix# #institutionType# of #institutionTarget#"

                    ;;IGU Types
                    igu "Elected Council" "Parliament of Hereditary Peers" "Appointed Council" "Overseer" "Ombudsman" "Judges" "A Warlord" "Representative Council" "Shareholders" "Tyrant" "Referendum"

                    ;;Enforcement
                    enforcers  "#incumbents#"
                    enforcement  "Enforcement is carried out by #role#, dictated by #incumbents.a#. <p> There is #amnt# corruption in the enforcers."


                    ;;Promotion
                    promotion  "ritual combat" "merit in relation to #tasks#" "the favour of superiors" "supplication to ones superiors" "debt" "sacrifice" "examination" "bribery" "threat" "assassination" "murder" "ritual" "time" "superiority" "racism" "subgroup membership" "heredity" "enthusiasm"
                    tasks  "institutional tasks" "personal tasks" "social tasks"

                    ;;Inter-field relations
                    stateRelation "The State #relationStrength# #stateRelationType# the Institution"
                    stateRelationType  "regulates" "observes" "controls" "authorises" "bans" "permit" "endorses" "licenses"
                    externalLinks  "#stateRelation#"

                    ;;Crises
                    crises  "inter-Incumbent friction" "incumbents trying to control challengers" "challengers ignoring rules until changed" "external pressure to improve the situation of challengers" "external pressure to control challengers" "external insertion of supported challengers" "revolutionary leader"

                    ;;Clothing
                    clothingAmnt   "minimal" "strict" "elaborative" "decorative" "regulated" "required" "optional" "open to interpretation"

                    clothingStandards   "The clothing standards are #clothingAmnt#. #clothingRules#."
                    clothingRules  "#clothingRule#, #clothingRule#, and #clothingRule#"
                    clothingTarget  "Incumbents" "Challengers" "Enforcers" "Leaders" "Minions" "Newcomers" "Old Hands" "No one"
                    clothingColour  "#institutionColour#" "#colour#"
                    clothing  "Hat" "Helm" "Coat" "Robe" "Suit" "Boot" "Glove" "Mask" "Right Hand Glove" "Left Hand Glove" "Cape" "Belt" "Trouser" "Skirt" "Miniskirt" "Dress"
                    clothingRule  "#clothingTarget# #deontic# wear #clothingColour# #clothing.s#"


                    ;;For General use:
                    role "Police" "Criminal" "Thief" "Murderer" "Wife" "Husband" "Secretary" "Commander" "Civilian" "Sergeant"
                    concepts  "#concept# #concept# and #concept#"
                    concept  "Money" "Connection" "Weapons" "Health" "Slavery"
                    relationStrength  "strictly" "loosely" "does not" "corruptly"
                    amnt  "no"  "some amount of"  "a lot of"  "high levels of"  "blatant"  "secretive"
                    deontic  "must"  "should"   "must not"  "may"  "should not"  "uniquely"
                    colour   "Black"  "Red"  "Yellow"  "Gold"  "Ochre"  "Green"  "Grey"  "Blue"  "Purple"
                    ))
      ;;----------------------------------------
      (test  (tracerel-grammar
              ;;Main Origin
              start "#attribute# #deonticMaybeSanction#"

              ;;Roles and people - who a norm applies to
              attribute  "#role#"  "#characteristic# people"  "#gender#"

              ;;Deontic statements, with s
              deonticMaybeSanction  "#deontic#"  "#deonticAndSanction#"
              deontic  "should #never# #aim# #conditions#" "could #never# #aim# #conditions#" "may #never# #aim# #conditions#"
              deonticAndSanction   "must #never# #aim# #conditions# #orElse#" "may not #never# #aim# #conditions# #orElse#" "have #duty# to #never# #aim# #conditions# #orElse#"

              ;;General use
              duty  "duty" "obligation"
              never  "never"  "never again"  "always"
              aim  "#privateDesignation# #action#" "#constitute#" "#membership#"
              privateDesignation  "publicly"  "privately"  ""
              the  ""  "the"  "a"  "the applicable"  "an applicable"
              gender  "Males"  "Females"  "Intersex"  "Non-binary people"  "Men"  "Women"
              role "Police" "Civilians" "Civil" "Servants" "Kings" "Criminals" "Librarians" "Shopkeepers" "Children" "Nuns"
              characteristic  "Brown Skinned"  "Light Skinned"  "Young"  "Old" "#colour.capitalize# eyed"  "Rich"  "Poor"  "Middle class"  "Working class" "#caste.capitalize# caste"  "Educated"  "Illiterate"  "Scruffy"  "Neat" "Gay"  "Straight"  "Bisexual"  "Short"  "Tall"
              caste "Upper" "middle" "lower" "untouchable"
              colour "red" "green" "blue" "grey" "purple"

              it  "it"  "#otherwise#"
              dont  "don't"  ""
              not  "not"  ""
              otherwise "otherwise"  "or else"  ""
              permission  "permits"  "forbids"
              items  "rare goods"  "ivory"  "food"  "water"  "beer"  "clothing"  "bread"  "spices" "pets"  "gems"  "gold"  "cattle"  "vegetables"  "wood"  "wool"

              locationDesignation  "the" "their" "a" "any" "some"
              location  "temple"  "shop"  "palace"  "river"  "whorehouse"  "mines"  "home"  "bathroom"  "prison"

              time  "Mid day" "Noon" "Morning" "Evening" "#moontype# moon"
              moontype  "Full"  "Waxing"  "Waning"  "New"  "Eclipsed"

              ;;Conditions
              conditions  "#if# #condition#" "#if# #condition# and #condition#" "#if# #condition# but not #if# #condition#" ""
              if  "if"  "unless"  "while"  "until"  "when"
              condition  "it is #not# #time#" "they have #instructed# #attribute#" "they #dont# have a permit" "they have #not# paid #the# tax" "the priesthood #permission# #it#"

              ;;Sanctions
              orElse  "#otherwise# they will be #besanction#" "#otherwise# they will have #havesanction#" ""
              besanction  "exiled"  "executed"  "tortured"  "whipped"  "placed in the stocks"  "shunned"  "arrested" "fined"  "mocked"  "outcast"  "excluded"
              havesanction   "their hand cut off"  "their bank account closed" "their property confiscated"  "to pay a fine"

              ;;Orders / instructions
              instructed  "#not# been instructed by" "#not# been ordered #otherwise# by" "#not# been given permission by" "#not# requested permission from" "#not# been forbidden to by"

              ;;Actions
              action  "wave to #attribute#" "buy #items#" "attack #attribute#" "kill #attribute#" "go to #locationDesignation# #location#" "report to the #locationDesignation# #location#" "steal from #attribute#" "fish #fishType#" "hunt #animal#" "farm #crops#" "read #bookType#" "weave #weaveType#" "paint #paintType#" "hire #workers#" "beg" "have #constraint# #numberChildren#" "pee"  "poo" "sleep #withPeople#" "make #items.s#" "pray #target#" "talk to #attribute#" "insult #personalRelation#'s #relMod##relation#"
              ;;Action Parameters
              fishType  "salmon"  "trout"  "pike"  "breem"  ""
              animal  "deer"  "bears"  "sheep"  "wolves"  "dogs"  "cats"  "rats"  ""
              crops  "wheat"  "maize"  "#drugs#"  ""
              drugs  "weed"  "poppies"  "heroin"  "cocaine"  "coffee"  "tea"  "opiates"

              workers  "prostitutes"  "labourers"  "chefs"  "maids"  "butlers"  "soldiers" "guards"  "clerks"  "secretaries"  "prisoners"  "janitors"

              weaveType  "clothes"  "capes"  "rugs"  "tapestries"
              paintType  "portraits"  "landscapes"  "abstract paintings"  "murals"  ""
              withPeople  "with #attribute#"  "with a #relMod##personalRelation#'s #relMod##relation#"
              target  ""  "to the #god#"

              personalRelation  "friend"  "lover"  "enemy"  "parent"  "child"  "acquaintance"
              "boss"  "subordinate"  "coworker"  "peer"  "someone"
              relation  "Mother"  "Father"  "Son"  "Daughter"  "Sister"  "Brother" "Lover"  "Lover"  "Husband"  "Wife"  "Ex-Wife"
              relMod  "Ex-" "Step-"  ""

              bookType   ""  "Religious books"  "Secular books"  "Political books" "Fiction"  "Non-Fiction"  "Cooking books"  "Maps" "History books"  "Foreign books"  "Philosophical books"

              god  "God of #godType#"  "Gods of #godType#"  "Pantheon of #godType#"  "Almighty"

              godType  "Fire"  "Water"  "the #time#"  "#location#"  "#items#" "#caste# caste"  "#colour#"

              constitute  "#not# be counted among #attribute#" "have the same #properties# as #attribute#"

              properties  "property rights"  "marriage rights"  "discrimination rights" "privileges"  "duties"

              constraint  "less than"  "more than"  "exactly"
              numberChildren  "one child"  "#number# children"
              number "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"

              membership  "be members of #guild#" "be affiliated with #guild#"

              guild  "The guild of #items#"
              ))
      )

  (message "%s" (tracerel-run (list test)))
  )
