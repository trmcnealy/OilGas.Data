// ReSharper disable InconsistentNaming

using System;

using Engineering.DataSource;

namespace OilGas.Data.RRC.Texas
{
    public static class ApiNumberExtensions
    {
        public static bool IsInCounty(this ApiNumber api,
                                      CountyType     county)
        {
            if(api.GetCountyCode() == county)
            {
                return true;
            }

            return false;
        }
    }

    public sealed class CountyType
    {
        public enum Kind : uint
        {
            None_Selected = 0,
            ANDERSON      = 1,
            ANDREWS       = 3,
            ANGELINA      = 5,
            ARANSAS       = 7,
            ARCHER        = 9,
            ARMSTRONG     = 11,
            ATASCOSA      = 13,
            AUSTIN        = 15,
            BAILEY        = 17,
            BANDERA       = 19,
            BASTROP       = 21,
            BAYLOR        = 23,
            BEE           = 25,
            BELL          = 27,
            BEXAR         = 29,
            BLANCO        = 31,
            BORDEN        = 33,
            BOSQUE        = 35,
            BOWIE         = 37,
            BRAZORIA      = 39,
            BRAZOS        = 41,
            BREWSTER      = 43,
            BRISCOE       = 45,
            BROOKS        = 47,
            BROWN         = 49,
            BURLESON      = 51,
            BURNET        = 53,
            CALDWELL      = 55,
            CALHOUN       = 57,
            CALLAHAN      = 59,
            CAMERON       = 61,
            CAMP          = 63,
            CARSON        = 65,
            CASS          = 67,
            CASTRO        = 69,
            CHAMBERS      = 71,
            CHEROKEE      = 73,
            CHILDRESS     = 75,
            CLAY          = 77,
            COCHRAN       = 79,
            COKE          = 81,
            COLEMAN       = 83,
            COLLIN        = 85,
            COLLINGSWORTH = 87,
            COLORADO      = 89,
            COMAL         = 91,
            COMANCHE      = 93,
            CONCHO        = 95,
            COOKE         = 97,
            CORYELL       = 99,
            COTTLE        = 101,
            CRANE         = 103,
            CROCKETT      = 105,
            CROSBY        = 107,
            CULBERSON     = 109,
            DALLAM        = 111,
            DALLAS        = 113,
            DAWSON        = 115,
            DEAF_SMITH    = 117,
            DELTA         = 119,
            DENTON        = 121,
            DE_WITT       = 123,
            DICKENS       = 125,
            DIMMIT        = 127,
            DONLEY        = 129,
            DUVAL         = 131,
            EASTLAND      = 133,
            ECTOR         = 135,
            EDWARDS       = 137,
            ELLIS         = 139,
            EL_PASO       = 141,
            ERATH         = 143,
            FALLS         = 145,
            FANNIN        = 147,
            FAYETTE       = 149,
            FISHER        = 151,
            FLOYD         = 153,
            FOARD         = 155,
            FORT_BEND     = 157,
            FRANKLIN      = 159,
            FREESTONE     = 161,
            FRIO          = 163,
            GAINES        = 165,
            GALVESTON     = 167,
            GARZA         = 169,
            GILLESPIE     = 171,
            GLASSCOCK     = 173,
            GOLIAD        = 175,
            GONZALES      = 177,
            GRAY          = 179,
            GRAYSON       = 181,
            GREGG         = 183,
            GRIMES        = 185,
            GUADALUPE     = 187,
            HALE          = 189,
            HALL          = 191,
            HAMILTON      = 193,
            HANSFORD      = 195,
            HARDEMAN      = 197,
            HARDIN        = 199,
            HARRIS        = 201,
            HARRISON      = 203,
            HARTLEY       = 205,
            HASKELL       = 207,
            HAYS          = 209,
            HEMPHILL      = 211,
            HENDERSON     = 213,
            HIDALGO       = 215,
            HILL          = 217,
            HOCKLEY       = 219,
            HOOD          = 221,
            HOPKINS       = 223,
            HOUSTON       = 225,
            HOWARD        = 227,
            HUDSPETH      = 229,
            HUNT          = 231,
            HUTCHINSON    = 233,
            IRION         = 235,
            JACK          = 237,
            JACKSON       = 239,
            JASPER        = 241,
            JEFF_DAVIS    = 243,
            JEFFERSON     = 245,
            JIM_HOGG      = 247,
            JIM_WELLS     = 249,
            JOHNSON       = 251,
            JONES         = 253,
            KARNES        = 255,
            KAUFMAN       = 257,
            KENDALL       = 259,
            KENEDY        = 261,
            KENT          = 263,
            KERR          = 265,
            KIMBLE        = 267,
            KING          = 269,
            KINNEY        = 271,
            KLEBERG       = 273,
            KNOX          = 275,
            LAMAR         = 277,
            LAMB          = 279,
            LAMPASAS      = 281,
            LA_SALLE      = 283,
            LAVACA        = 285,
            LEE           = 287,
            LEON          = 289,
            LIBERTY       = 291,
            LIMESTONE     = 293,
            LIPSCOMB      = 295,
            LIVE_OAK      = 297,
            LLANO         = 299,
            LOVING        = 301,
            LUBBOCK       = 303,
            LYNN          = 305,
            MCCULLOCH     = 307,
            MCLENNAN      = 309,
            MCMULLEN      = 311,
            MADISON       = 313,
            MARION        = 315,
            MARTIN        = 317,
            MASON         = 319,
            MATAGORDA     = 321,
            MAVERICK      = 323,
            MEDINA        = 325,
            MENARD        = 327,
            MIDLAND       = 329,
            MILAM         = 331,
            MILLS         = 333,
            MITCHELL      = 335,
            MONTAGUE      = 337,
            MONTGOMERY    = 339,
            MOORE         = 341,
            MORRIS        = 343,
            MOTLEY        = 345,
            NACOGDOCHES   = 347,
            NAVARRO       = 349,
            NEWTON        = 351,
            NOLAN         = 353,
            NUECES        = 355,
            OCHILTREE     = 357,
            OLDHAM        = 359,
            ORANGE        = 361,
            PALO_PINTO    = 363,
            PANOLA        = 365,
            PARKER        = 367,
            PARMER        = 369,
            PECOS         = 371,
            POLK          = 373,
            POTTER        = 375,
            PRESIDIO      = 377,
            RAINS         = 379,
            RANDALL       = 381,
            REAGAN        = 383,
            REAL          = 385,
            RED_RIVER     = 387,
            REEVES        = 389,
            REFUGIO       = 391,
            ROBERTS       = 393,
            ROBERTSON     = 395,
            ROCKWALL      = 397,
            RUNNELS       = 399,
            RUSK          = 401,
            SABINE        = 403,
            SAN_AUGUSTINE = 405,
            SAN_JACINTO   = 407,
            SAN_PATRICIO  = 409,
            SAN_SABA      = 411,
            SCHLEICHER    = 413,
            SCURRY        = 415,
            SHACKELFORD   = 417,
            SHELBY        = 419,
            SHERMAN       = 421,
            SMITH         = 423,
            SOMERVELL     = 425,
            STARR         = 427,
            STEPHENS      = 429,
            STERLING      = 431,
            STONEWALL     = 433,
            SUTTON        = 435,
            SWISHER       = 437,
            TARRANT       = 439,
            TAYLOR        = 441,
            TERRELL       = 443,
            TERRY         = 445,
            THROCKMORTON  = 447,
            TITUS         = 449,
            TOM_GREEN     = 451,
            TRAVIS        = 453,
            TRINITY       = 455,
            TYLER         = 457,
            UPSHUR        = 459,
            UPTON         = 461,
            UVALDE        = 463,
            VAL_VERDE     = 465,
            VAN_ZANDT     = 467,
            VICTORIA      = 469,
            WALKER        = 471,
            WALLER        = 473,
            WARD          = 475,
            WASHINGTON    = 477,
            WEBB          = 479,
            WHARTON       = 481,
            WHEELER       = 483,
            WICHITA       = 485,
            WILBARGER     = 487,
            WILLACY       = 489,
            WILLIAMSON    = 491,
            WILSON        = 493,
            WINKLER       = 495,
            WISE          = 497,
            WOOD          = 499,
            YOAKUM        = 501,
            YOUNG         = 503,
            ZAPATA        = 505,
            ZAVALA        = 507,
            S_PADRE_IS_SB = 600,
            N_PADRE_IS_SB = 601,
            MUSTANG_IS_SB = 602,
            MATGRDA_IS_SB = 603,
            BRAZOS_SB     = 604,
            GALVESTON_SB  = 605,
            HIGH_IS_SB    = 606,
            S_PADRE_IS_LB = 700,
            N_PADRE_IS_LB = 701,
            MUSTANG_IS_LB = 702,
            MATGRDA_IS_LB = 703,
            BRAZOS_LB     = 704,
            BRAZOS_S      = 705,
            GALVESTON_LB  = 706,
            GALVESTON_S   = 707,
            HIGH_IS_LB    = 708,
            HIGH_IS_S     = 709,
            HIGH_IS_E     = 710,
            HIGH_IS_E_S   = 711,
            MUSTANG_IS_E  = 712,
            N_PADRE_IS_E  = 713,
            S_PADRE_IS_E  = 714,
            SABINE_PASS   = 715
        }

        public Kind Value { get; set; }

        public CountyType()
        {
            Value = Kind.None_Selected;
        }

        public CountyType(Kind value)
        {
            Value = value;
        }

        public CountyType(string value)
        {
            Value = FromName(value);
        }

        public static implicit operator CountyType(Kind code)
        {
            return new CountyType(code);
        }

        public static implicit operator int(CountyType districtCode)
        {
            return (int)districtCode.Value;
        }

        public static implicit operator string(CountyType districtCode)
        {
            return districtCode.ToString();
        }

        public override string ToString()
        {
            return GetName(Value);
        }
        
        public static string GetName(Kind kind)
        {
            switch(kind)
            {
                case Kind.None_Selected: return "None Selected";
                case Kind.ANDERSON:      return "ANDERSON";
                case Kind.ANDREWS:       return "ANDREWS";
                case Kind.ANGELINA:      return "ANGELINA";
                case Kind.ARANSAS:       return "ARANSAS";
                case Kind.ARCHER:        return "ARCHER";
                case Kind.ARMSTRONG:     return "ARMSTRONG";
                case Kind.ATASCOSA:      return "ATASCOSA";
                case Kind.AUSTIN:        return "AUSTIN";
                case Kind.BAILEY:        return "BAILEY";
                case Kind.BANDERA:       return "BANDERA";
                case Kind.BASTROP:       return "BASTROP";
                case Kind.BAYLOR:        return "BAYLOR";
                case Kind.BEE:           return "BEE";
                case Kind.BELL:          return "BELL";
                case Kind.BEXAR:         return "BEXAR";
                case Kind.BLANCO:        return "BLANCO";
                case Kind.BORDEN:        return "BORDEN";
                case Kind.BOSQUE:        return "BOSQUE";
                case Kind.BOWIE:         return "BOWIE";
                case Kind.BRAZORIA:      return "BRAZORIA";
                case Kind.BRAZOS:        return "BRAZOS";
                case Kind.BREWSTER:      return "BREWSTER";
                case Kind.BRISCOE:       return "BRISCOE";
                case Kind.BROOKS:        return "BROOKS";
                case Kind.BROWN:         return "BROWN";
                case Kind.BURLESON:      return "BURLESON";
                case Kind.BURNET:        return "BURNET";
                case Kind.CALDWELL:      return "CALDWELL";
                case Kind.CALHOUN:       return "CALHOUN";
                case Kind.CALLAHAN:      return "CALLAHAN";
                case Kind.CAMERON:       return "CAMERON";
                case Kind.CAMP:          return "CAMP";
                case Kind.CARSON:        return "CARSON";
                case Kind.CASS:          return "CASS";
                case Kind.CASTRO:        return "CASTRO";
                case Kind.CHAMBERS:      return "CHAMBERS";
                case Kind.CHEROKEE:      return "CHEROKEE";
                case Kind.CHILDRESS:     return "CHILDRESS";
                case Kind.CLAY:          return "CLAY";
                case Kind.COCHRAN:       return "COCHRAN";
                case Kind.COKE:          return "COKE";
                case Kind.COLEMAN:       return "COLEMAN";
                case Kind.COLLIN:        return "COLLIN";
                case Kind.COLLINGSWORTH: return "COLLINGSWORTH";
                case Kind.COLORADO:      return "COLORADO";
                case Kind.COMAL:         return "COMAL";
                case Kind.COMANCHE:      return "COMANCHE";
                case Kind.CONCHO:        return "CONCHO";
                case Kind.COOKE:         return "COOKE";
                case Kind.CORYELL:       return "CORYELL";
                case Kind.COTTLE:        return "COTTLE";
                case Kind.CRANE:         return "CRANE";
                case Kind.CROCKETT:      return "CROCKETT";
                case Kind.CROSBY:        return "CROSBY";
                case Kind.CULBERSON:     return "CULBERSON";
                case Kind.DALLAM:        return "DALLAM";
                case Kind.DALLAS:        return "DALLAS";
                case Kind.DAWSON:        return "DAWSON";
                case Kind.DEAF_SMITH:    return "DEAF SMITH";
                case Kind.DELTA:         return "DELTA";
                case Kind.DENTON:        return "DENTON";
                case Kind.DE_WITT:       return "DE WITT";
                case Kind.DICKENS:       return "DICKENS";
                case Kind.DIMMIT:        return "DIMMIT";
                case Kind.DONLEY:        return "DONLEY";
                case Kind.DUVAL:         return "DUVAL";
                case Kind.EASTLAND:      return "EASTLAND";
                case Kind.ECTOR:         return "ECTOR";
                case Kind.EDWARDS:       return "EDWARDS";
                case Kind.ELLIS:         return "ELLIS";
                case Kind.EL_PASO:       return "EL PASO";
                case Kind.ERATH:         return "ERATH";
                case Kind.FALLS:         return "FALLS";
                case Kind.FANNIN:        return "FANNIN";
                case Kind.FAYETTE:       return "FAYETTE";
                case Kind.FISHER:        return "FISHER";
                case Kind.FLOYD:         return "FLOYD";
                case Kind.FOARD:         return "FOARD";
                case Kind.FORT_BEND:     return "FORT BEND";
                case Kind.FRANKLIN:      return "FRANKLIN";
                case Kind.FREESTONE:     return "FREESTONE";
                case Kind.FRIO:          return "FRIO";
                case Kind.GAINES:        return "GAINES";
                case Kind.GALVESTON:     return "GALVESTON";
                case Kind.GARZA:         return "GARZA";
                case Kind.GILLESPIE:     return "GILLESPIE";
                case Kind.GLASSCOCK:     return "GLASSCOCK";
                case Kind.GOLIAD:        return "GOLIAD";
                case Kind.GONZALES:      return "GONZALES";
                case Kind.GRAY:          return "GRAY";
                case Kind.GRAYSON:       return "GRAYSON";
                case Kind.GREGG:         return "GREGG";
                case Kind.GRIMES:        return "GRIMES";
                case Kind.GUADALUPE:     return "GUADALUPE";
                case Kind.HALE:          return "HALE";
                case Kind.HALL:          return "HALL";
                case Kind.HAMILTON:      return "HAMILTON";
                case Kind.HANSFORD:      return "HANSFORD";
                case Kind.HARDEMAN:      return "HARDEMAN";
                case Kind.HARDIN:        return "HARDIN";
                case Kind.HARRIS:        return "HARRIS";
                case Kind.HARRISON:      return "HARRISON";
                case Kind.HARTLEY:       return "HARTLEY";
                case Kind.HASKELL:       return "HASKELL";
                case Kind.HAYS:          return "HAYS";
                case Kind.HEMPHILL:      return "HEMPHILL";
                case Kind.HENDERSON:     return "HENDERSON";
                case Kind.HIDALGO:       return "HIDALGO";
                case Kind.HILL:          return "HILL";
                case Kind.HOCKLEY:       return "HOCKLEY";
                case Kind.HOOD:          return "HOOD";
                case Kind.HOPKINS:       return "HOPKINS";
                case Kind.HOUSTON:       return "HOUSTON";
                case Kind.HOWARD:        return "HOWARD";
                case Kind.HUDSPETH:      return "HUDSPETH";
                case Kind.HUNT:          return "HUNT";
                case Kind.HUTCHINSON:    return "HUTCHINSON";
                case Kind.IRION:         return "IRION";
                case Kind.JACK:          return "JACK";
                case Kind.JACKSON:       return "JACKSON";
                case Kind.JASPER:        return "JASPER";
                case Kind.JEFF_DAVIS:    return "JEFF DAVIS";
                case Kind.JEFFERSON:     return "JEFFERSON";
                case Kind.JIM_HOGG:      return "JIM HOGG";
                case Kind.JIM_WELLS:     return "JIM WELLS";
                case Kind.JOHNSON:       return "JOHNSON";
                case Kind.JONES:         return "JONES";
                case Kind.KARNES:        return "KARNES";
                case Kind.KAUFMAN:       return "KAUFMAN";
                case Kind.KENDALL:       return "KENDALL";
                case Kind.KENEDY:        return "KENEDY";
                case Kind.KENT:          return "KENT";
                case Kind.KERR:          return "KERR";
                case Kind.KIMBLE:        return "KIMBLE";
                case Kind.KING:          return "KING";
                case Kind.KINNEY:        return "KINNEY";
                case Kind.KLEBERG:       return "KLEBERG";
                case Kind.KNOX:          return "KNOX";
                case Kind.LAMAR:         return "LAMAR";
                case Kind.LAMB:          return "LAMB";
                case Kind.LAMPASAS:      return "LAMPASAS";
                case Kind.LA_SALLE:      return "LA SALLE";
                case Kind.LAVACA:        return "LAVACA";
                case Kind.LEE:           return "LEE";
                case Kind.LEON:          return "LEON";
                case Kind.LIBERTY:       return "LIBERTY";
                case Kind.LIMESTONE:     return "LIMESTONE";
                case Kind.LIPSCOMB:      return "LIPSCOMB";
                case Kind.LIVE_OAK:      return "LIVE OAK";
                case Kind.LLANO:         return "LLANO";
                case Kind.LOVING:        return "LOVING";
                case Kind.LUBBOCK:       return "LUBBOCK";
                case Kind.LYNN:          return "LYNN";
                case Kind.MCCULLOCH:     return "MCCULLOCH";
                case Kind.MCLENNAN:      return "MCLENNAN";
                case Kind.MCMULLEN:      return "MCMULLEN";
                case Kind.MADISON:       return "MADISON";
                case Kind.MARION:        return "MARION";
                case Kind.MARTIN:        return "MARTIN";
                case Kind.MASON:         return "MASON";
                case Kind.MATAGORDA:     return "MATAGORDA";
                case Kind.MAVERICK:      return "MAVERICK";
                case Kind.MEDINA:        return "MEDINA";
                case Kind.MENARD:        return "MENARD";
                case Kind.MIDLAND:       return "MIDLAND";
                case Kind.MILAM:         return "MILAM";
                case Kind.MILLS:         return "MILLS";
                case Kind.MITCHELL:      return "MITCHELL";
                case Kind.MONTAGUE:      return "MONTAGUE";
                case Kind.MONTGOMERY:    return "MONTGOMERY";
                case Kind.MOORE:         return "MOORE";
                case Kind.MORRIS:        return "MORRIS";
                case Kind.MOTLEY:        return "MOTLEY";
                case Kind.NACOGDOCHES:   return "NACOGDOCHES";
                case Kind.NAVARRO:       return "NAVARRO";
                case Kind.NEWTON:        return "NEWTON";
                case Kind.NOLAN:         return "NOLAN";
                case Kind.NUECES:        return "NUECES";
                case Kind.OCHILTREE:     return "OCHILTREE";
                case Kind.OLDHAM:        return "OLDHAM";
                case Kind.ORANGE:        return "ORANGE";
                case Kind.PALO_PINTO:    return "PALO PINTO";
                case Kind.PANOLA:        return "PANOLA";
                case Kind.PARKER:        return "PARKER";
                case Kind.PARMER:        return "PARMER";
                case Kind.PECOS:         return "PECOS";
                case Kind.POLK:          return "POLK";
                case Kind.POTTER:        return "POTTER";
                case Kind.PRESIDIO:      return "PRESIDIO";
                case Kind.RAINS:         return "RAINS";
                case Kind.RANDALL:       return "RANDALL";
                case Kind.REAGAN:        return "REAGAN";
                case Kind.REAL:          return "REAL";
                case Kind.RED_RIVER:     return "RED RIVER";
                case Kind.REEVES:        return "REEVES";
                case Kind.REFUGIO:       return "REFUGIO";
                case Kind.ROBERTS:       return "ROBERTS";
                case Kind.ROBERTSON:     return "ROBERTSON";
                case Kind.ROCKWALL:      return "ROCKWALL";
                case Kind.RUNNELS:       return "RUNNELS";
                case Kind.RUSK:          return "RUSK";
                case Kind.SABINE:        return "SABINE";
                case Kind.SAN_AUGUSTINE: return "SAN AUGUSTINE";
                case Kind.SAN_JACINTO:   return "SAN JACINTO";
                case Kind.SAN_PATRICIO:  return "SAN PATRICIO";
                case Kind.SAN_SABA:      return "SAN SABA";
                case Kind.SCHLEICHER:    return "SCHLEICHER";
                case Kind.SCURRY:        return "SCURRY";
                case Kind.SHACKELFORD:   return "SHACKELFORD";
                case Kind.SHELBY:        return "SHELBY";
                case Kind.SHERMAN:       return "SHERMAN";
                case Kind.SMITH:         return "SMITH";
                case Kind.SOMERVELL:     return "SOMERVELL";
                case Kind.STARR:         return "STARR";
                case Kind.STEPHENS:      return "STEPHENS";
                case Kind.STERLING:      return "STERLING";
                case Kind.STONEWALL:     return "STONEWALL";
                case Kind.SUTTON:        return "SUTTON";
                case Kind.SWISHER:       return "SWISHER";
                case Kind.TARRANT:       return "TARRANT";
                case Kind.TAYLOR:        return "TAYLOR";
                case Kind.TERRELL:       return "TERRELL";
                case Kind.TERRY:         return "TERRY";
                case Kind.THROCKMORTON:  return "THROCKMORTON";
                case Kind.TITUS:         return "TITUS";
                case Kind.TOM_GREEN:     return "TOM GREEN";
                case Kind.TRAVIS:        return "TRAVIS";
                case Kind.TRINITY:       return "TRINITY";
                case Kind.TYLER:         return "TYLER";
                case Kind.UPSHUR:        return "UPSHUR";
                case Kind.UPTON:         return "UPTON";
                case Kind.UVALDE:        return "UVALDE";
                case Kind.VAL_VERDE:     return "VAL VERDE";
                case Kind.VAN_ZANDT:     return "VAN ZANDT";
                case Kind.VICTORIA:      return "VICTORIA";
                case Kind.WALKER:        return "WALKER";
                case Kind.WALLER:        return "WALLER";
                case Kind.WARD:          return "WARD";
                case Kind.WASHINGTON:    return "WASHINGTON";
                case Kind.WEBB:          return "WEBB";
                case Kind.WHARTON:       return "WHARTON";
                case Kind.WHEELER:       return "WHEELER";
                case Kind.WICHITA:       return "WICHITA";
                case Kind.WILBARGER:     return "WILBARGER";
                case Kind.WILLACY:       return "WILLACY";
                case Kind.WILLIAMSON:    return "WILLIAMSON";
                case Kind.WILSON:        return "WILSON";
                case Kind.WINKLER:       return "WINKLER";
                case Kind.WISE:          return "WISE";
                case Kind.WOOD:          return "WOOD";
                case Kind.YOAKUM:        return "YOAKUM";
                case Kind.YOUNG:         return "YOUNG";
                case Kind.ZAPATA:        return "ZAPATA";
                case Kind.ZAVALA:        return "ZAVALA";
                case Kind.S_PADRE_IS_SB: return "S PADRE IS-SB";
                case Kind.N_PADRE_IS_SB: return "N PADRE IS-SB";
                case Kind.MUSTANG_IS_SB: return "MUSTANG IS-SB";
                case Kind.MATGRDA_IS_SB: return "MATGRDA IS-SB";
                case Kind.BRAZOS_SB:     return "BRAZOS-SB";
                case Kind.GALVESTON_SB:  return "GALVESTON-SB";
                case Kind.HIGH_IS_SB:    return "HIGH IS-SB";
                case Kind.S_PADRE_IS_LB: return "S PADRE IS-LB";
                case Kind.N_PADRE_IS_LB: return "N PADRE IS-LB";
                case Kind.MUSTANG_IS_LB: return "MUSTANG IS-LB";
                case Kind.MATGRDA_IS_LB: return "MATGRDA IS-LB";
                case Kind.BRAZOS_LB:     return "BRAZOS-LB";
                case Kind.BRAZOS_S:      return "BRAZOS-S";
                case Kind.GALVESTON_LB:  return "GALVESTON-LB";
                case Kind.GALVESTON_S:   return "GALVESTON-S";
                case Kind.HIGH_IS_LB:    return "HIGH IS-LB";
                case Kind.HIGH_IS_S:     return "HIGH IS-S";
                case Kind.HIGH_IS_E:     return "HIGH IS-E";
                case Kind.HIGH_IS_E_S:   return "HIGH IS-E,S";
                case Kind.MUSTANG_IS_E:  return "MUSTANG IS-E";
                case Kind.N_PADRE_IS_E:  return "N PADRE IS-E";
                case Kind.S_PADRE_IS_E:  return "S PADRE IS-E";
                case Kind.SABINE_PASS:   return "SABINE PASS";
                default:                 throw new ArgumentOutOfRangeException(nameof(kind), kind, null);
            }
        }

        public static string GetName(uint kind)
        {
            switch((Kind)kind)
            {
                case Kind.None_Selected: return "None Selected";
                case Kind.ANDERSON:      return "ANDERSON";
                case Kind.ANDREWS:       return "ANDREWS";
                case Kind.ANGELINA:      return "ANGELINA";
                case Kind.ARANSAS:       return "ARANSAS";
                case Kind.ARCHER:        return "ARCHER";
                case Kind.ARMSTRONG:     return "ARMSTRONG";
                case Kind.ATASCOSA:      return "ATASCOSA";
                case Kind.AUSTIN:        return "AUSTIN";
                case Kind.BAILEY:        return "BAILEY";
                case Kind.BANDERA:       return "BANDERA";
                case Kind.BASTROP:       return "BASTROP";
                case Kind.BAYLOR:        return "BAYLOR";
                case Kind.BEE:           return "BEE";
                case Kind.BELL:          return "BELL";
                case Kind.BEXAR:         return "BEXAR";
                case Kind.BLANCO:        return "BLANCO";
                case Kind.BORDEN:        return "BORDEN";
                case Kind.BOSQUE:        return "BOSQUE";
                case Kind.BOWIE:         return "BOWIE";
                case Kind.BRAZORIA:      return "BRAZORIA";
                case Kind.BRAZOS:        return "BRAZOS";
                case Kind.BREWSTER:      return "BREWSTER";
                case Kind.BRISCOE:       return "BRISCOE";
                case Kind.BROOKS:        return "BROOKS";
                case Kind.BROWN:         return "BROWN";
                case Kind.BURLESON:      return "BURLESON";
                case Kind.BURNET:        return "BURNET";
                case Kind.CALDWELL:      return "CALDWELL";
                case Kind.CALHOUN:       return "CALHOUN";
                case Kind.CALLAHAN:      return "CALLAHAN";
                case Kind.CAMERON:       return "CAMERON";
                case Kind.CAMP:          return "CAMP";
                case Kind.CARSON:        return "CARSON";
                case Kind.CASS:          return "CASS";
                case Kind.CASTRO:        return "CASTRO";
                case Kind.CHAMBERS:      return "CHAMBERS";
                case Kind.CHEROKEE:      return "CHEROKEE";
                case Kind.CHILDRESS:     return "CHILDRESS";
                case Kind.CLAY:          return "CLAY";
                case Kind.COCHRAN:       return "COCHRAN";
                case Kind.COKE:          return "COKE";
                case Kind.COLEMAN:       return "COLEMAN";
                case Kind.COLLIN:        return "COLLIN";
                case Kind.COLLINGSWORTH: return "COLLINGSWORTH";
                case Kind.COLORADO:      return "COLORADO";
                case Kind.COMAL:         return "COMAL";
                case Kind.COMANCHE:      return "COMANCHE";
                case Kind.CONCHO:        return "CONCHO";
                case Kind.COOKE:         return "COOKE";
                case Kind.CORYELL:       return "CORYELL";
                case Kind.COTTLE:        return "COTTLE";
                case Kind.CRANE:         return "CRANE";
                case Kind.CROCKETT:      return "CROCKETT";
                case Kind.CROSBY:        return "CROSBY";
                case Kind.CULBERSON:     return "CULBERSON";
                case Kind.DALLAM:        return "DALLAM";
                case Kind.DALLAS:        return "DALLAS";
                case Kind.DAWSON:        return "DAWSON";
                case Kind.DEAF_SMITH:    return "DEAF SMITH";
                case Kind.DELTA:         return "DELTA";
                case Kind.DENTON:        return "DENTON";
                case Kind.DE_WITT:       return "DE WITT";
                case Kind.DICKENS:       return "DICKENS";
                case Kind.DIMMIT:        return "DIMMIT";
                case Kind.DONLEY:        return "DONLEY";
                case Kind.DUVAL:         return "DUVAL";
                case Kind.EASTLAND:      return "EASTLAND";
                case Kind.ECTOR:         return "ECTOR";
                case Kind.EDWARDS:       return "EDWARDS";
                case Kind.ELLIS:         return "ELLIS";
                case Kind.EL_PASO:       return "EL PASO";
                case Kind.ERATH:         return "ERATH";
                case Kind.FALLS:         return "FALLS";
                case Kind.FANNIN:        return "FANNIN";
                case Kind.FAYETTE:       return "FAYETTE";
                case Kind.FISHER:        return "FISHER";
                case Kind.FLOYD:         return "FLOYD";
                case Kind.FOARD:         return "FOARD";
                case Kind.FORT_BEND:     return "FORT BEND";
                case Kind.FRANKLIN:      return "FRANKLIN";
                case Kind.FREESTONE:     return "FREESTONE";
                case Kind.FRIO:          return "FRIO";
                case Kind.GAINES:        return "GAINES";
                case Kind.GALVESTON:     return "GALVESTON";
                case Kind.GARZA:         return "GARZA";
                case Kind.GILLESPIE:     return "GILLESPIE";
                case Kind.GLASSCOCK:     return "GLASSCOCK";
                case Kind.GOLIAD:        return "GOLIAD";
                case Kind.GONZALES:      return "GONZALES";
                case Kind.GRAY:          return "GRAY";
                case Kind.GRAYSON:       return "GRAYSON";
                case Kind.GREGG:         return "GREGG";
                case Kind.GRIMES:        return "GRIMES";
                case Kind.GUADALUPE:     return "GUADALUPE";
                case Kind.HALE:          return "HALE";
                case Kind.HALL:          return "HALL";
                case Kind.HAMILTON:      return "HAMILTON";
                case Kind.HANSFORD:      return "HANSFORD";
                case Kind.HARDEMAN:      return "HARDEMAN";
                case Kind.HARDIN:        return "HARDIN";
                case Kind.HARRIS:        return "HARRIS";
                case Kind.HARRISON:      return "HARRISON";
                case Kind.HARTLEY:       return "HARTLEY";
                case Kind.HASKELL:       return "HASKELL";
                case Kind.HAYS:          return "HAYS";
                case Kind.HEMPHILL:      return "HEMPHILL";
                case Kind.HENDERSON:     return "HENDERSON";
                case Kind.HIDALGO:       return "HIDALGO";
                case Kind.HILL:          return "HILL";
                case Kind.HOCKLEY:       return "HOCKLEY";
                case Kind.HOOD:          return "HOOD";
                case Kind.HOPKINS:       return "HOPKINS";
                case Kind.HOUSTON:       return "HOUSTON";
                case Kind.HOWARD:        return "HOWARD";
                case Kind.HUDSPETH:      return "HUDSPETH";
                case Kind.HUNT:          return "HUNT";
                case Kind.HUTCHINSON:    return "HUTCHINSON";
                case Kind.IRION:         return "IRION";
                case Kind.JACK:          return "JACK";
                case Kind.JACKSON:       return "JACKSON";
                case Kind.JASPER:        return "JASPER";
                case Kind.JEFF_DAVIS:    return "JEFF DAVIS";
                case Kind.JEFFERSON:     return "JEFFERSON";
                case Kind.JIM_HOGG:      return "JIM HOGG";
                case Kind.JIM_WELLS:     return "JIM WELLS";
                case Kind.JOHNSON:       return "JOHNSON";
                case Kind.JONES:         return "JONES";
                case Kind.KARNES:        return "KARNES";
                case Kind.KAUFMAN:       return "KAUFMAN";
                case Kind.KENDALL:       return "KENDALL";
                case Kind.KENEDY:        return "KENEDY";
                case Kind.KENT:          return "KENT";
                case Kind.KERR:          return "KERR";
                case Kind.KIMBLE:        return "KIMBLE";
                case Kind.KING:          return "KING";
                case Kind.KINNEY:        return "KINNEY";
                case Kind.KLEBERG:       return "KLEBERG";
                case Kind.KNOX:          return "KNOX";
                case Kind.LAMAR:         return "LAMAR";
                case Kind.LAMB:          return "LAMB";
                case Kind.LAMPASAS:      return "LAMPASAS";
                case Kind.LA_SALLE:      return "LA SALLE";
                case Kind.LAVACA:        return "LAVACA";
                case Kind.LEE:           return "LEE";
                case Kind.LEON:          return "LEON";
                case Kind.LIBERTY:       return "LIBERTY";
                case Kind.LIMESTONE:     return "LIMESTONE";
                case Kind.LIPSCOMB:      return "LIPSCOMB";
                case Kind.LIVE_OAK:      return "LIVE OAK";
                case Kind.LLANO:         return "LLANO";
                case Kind.LOVING:        return "LOVING";
                case Kind.LUBBOCK:       return "LUBBOCK";
                case Kind.LYNN:          return "LYNN";
                case Kind.MCCULLOCH:     return "MCCULLOCH";
                case Kind.MCLENNAN:      return "MCLENNAN";
                case Kind.MCMULLEN:      return "MCMULLEN";
                case Kind.MADISON:       return "MADISON";
                case Kind.MARION:        return "MARION";
                case Kind.MARTIN:        return "MARTIN";
                case Kind.MASON:         return "MASON";
                case Kind.MATAGORDA:     return "MATAGORDA";
                case Kind.MAVERICK:      return "MAVERICK";
                case Kind.MEDINA:        return "MEDINA";
                case Kind.MENARD:        return "MENARD";
                case Kind.MIDLAND:       return "MIDLAND";
                case Kind.MILAM:         return "MILAM";
                case Kind.MILLS:         return "MILLS";
                case Kind.MITCHELL:      return "MITCHELL";
                case Kind.MONTAGUE:      return "MONTAGUE";
                case Kind.MONTGOMERY:    return "MONTGOMERY";
                case Kind.MOORE:         return "MOORE";
                case Kind.MORRIS:        return "MORRIS";
                case Kind.MOTLEY:        return "MOTLEY";
                case Kind.NACOGDOCHES:   return "NACOGDOCHES";
                case Kind.NAVARRO:       return "NAVARRO";
                case Kind.NEWTON:        return "NEWTON";
                case Kind.NOLAN:         return "NOLAN";
                case Kind.NUECES:        return "NUECES";
                case Kind.OCHILTREE:     return "OCHILTREE";
                case Kind.OLDHAM:        return "OLDHAM";
                case Kind.ORANGE:        return "ORANGE";
                case Kind.PALO_PINTO:    return "PALO PINTO";
                case Kind.PANOLA:        return "PANOLA";
                case Kind.PARKER:        return "PARKER";
                case Kind.PARMER:        return "PARMER";
                case Kind.PECOS:         return "PECOS";
                case Kind.POLK:          return "POLK";
                case Kind.POTTER:        return "POTTER";
                case Kind.PRESIDIO:      return "PRESIDIO";
                case Kind.RAINS:         return "RAINS";
                case Kind.RANDALL:       return "RANDALL";
                case Kind.REAGAN:        return "REAGAN";
                case Kind.REAL:          return "REAL";
                case Kind.RED_RIVER:     return "RED RIVER";
                case Kind.REEVES:        return "REEVES";
                case Kind.REFUGIO:       return "REFUGIO";
                case Kind.ROBERTS:       return "ROBERTS";
                case Kind.ROBERTSON:     return "ROBERTSON";
                case Kind.ROCKWALL:      return "ROCKWALL";
                case Kind.RUNNELS:       return "RUNNELS";
                case Kind.RUSK:          return "RUSK";
                case Kind.SABINE:        return "SABINE";
                case Kind.SAN_AUGUSTINE: return "SAN AUGUSTINE";
                case Kind.SAN_JACINTO:   return "SAN JACINTO";
                case Kind.SAN_PATRICIO:  return "SAN PATRICIO";
                case Kind.SAN_SABA:      return "SAN SABA";
                case Kind.SCHLEICHER:    return "SCHLEICHER";
                case Kind.SCURRY:        return "SCURRY";
                case Kind.SHACKELFORD:   return "SHACKELFORD";
                case Kind.SHELBY:        return "SHELBY";
                case Kind.SHERMAN:       return "SHERMAN";
                case Kind.SMITH:         return "SMITH";
                case Kind.SOMERVELL:     return "SOMERVELL";
                case Kind.STARR:         return "STARR";
                case Kind.STEPHENS:      return "STEPHENS";
                case Kind.STERLING:      return "STERLING";
                case Kind.STONEWALL:     return "STONEWALL";
                case Kind.SUTTON:        return "SUTTON";
                case Kind.SWISHER:       return "SWISHER";
                case Kind.TARRANT:       return "TARRANT";
                case Kind.TAYLOR:        return "TAYLOR";
                case Kind.TERRELL:       return "TERRELL";
                case Kind.TERRY:         return "TERRY";
                case Kind.THROCKMORTON:  return "THROCKMORTON";
                case Kind.TITUS:         return "TITUS";
                case Kind.TOM_GREEN:     return "TOM GREEN";
                case Kind.TRAVIS:        return "TRAVIS";
                case Kind.TRINITY:       return "TRINITY";
                case Kind.TYLER:         return "TYLER";
                case Kind.UPSHUR:        return "UPSHUR";
                case Kind.UPTON:         return "UPTON";
                case Kind.UVALDE:        return "UVALDE";
                case Kind.VAL_VERDE:     return "VAL VERDE";
                case Kind.VAN_ZANDT:     return "VAN ZANDT";
                case Kind.VICTORIA:      return "VICTORIA";
                case Kind.WALKER:        return "WALKER";
                case Kind.WALLER:        return "WALLER";
                case Kind.WARD:          return "WARD";
                case Kind.WASHINGTON:    return "WASHINGTON";
                case Kind.WEBB:          return "WEBB";
                case Kind.WHARTON:       return "WHARTON";
                case Kind.WHEELER:       return "WHEELER";
                case Kind.WICHITA:       return "WICHITA";
                case Kind.WILBARGER:     return "WILBARGER";
                case Kind.WILLACY:       return "WILLACY";
                case Kind.WILLIAMSON:    return "WILLIAMSON";
                case Kind.WILSON:        return "WILSON";
                case Kind.WINKLER:       return "WINKLER";
                case Kind.WISE:          return "WISE";
                case Kind.WOOD:          return "WOOD";
                case Kind.YOAKUM:        return "YOAKUM";
                case Kind.YOUNG:         return "YOUNG";
                case Kind.ZAPATA:        return "ZAPATA";
                case Kind.ZAVALA:        return "ZAVALA";
                case Kind.S_PADRE_IS_SB: return "S PADRE IS-SB";
                case Kind.N_PADRE_IS_SB: return "N PADRE IS-SB";
                case Kind.MUSTANG_IS_SB: return "MUSTANG IS-SB";
                case Kind.MATGRDA_IS_SB: return "MATGRDA IS-SB";
                case Kind.BRAZOS_SB:     return "BRAZOS-SB";
                case Kind.GALVESTON_SB:  return "GALVESTON-SB";
                case Kind.HIGH_IS_SB:    return "HIGH IS-SB";
                case Kind.S_PADRE_IS_LB: return "S PADRE IS-LB";
                case Kind.N_PADRE_IS_LB: return "N PADRE IS-LB";
                case Kind.MUSTANG_IS_LB: return "MUSTANG IS-LB";
                case Kind.MATGRDA_IS_LB: return "MATGRDA IS-LB";
                case Kind.BRAZOS_LB:     return "BRAZOS-LB";
                case Kind.BRAZOS_S:      return "BRAZOS-S";
                case Kind.GALVESTON_LB:  return "GALVESTON-LB";
                case Kind.GALVESTON_S:   return "GALVESTON-S";
                case Kind.HIGH_IS_LB:    return "HIGH IS-LB";
                case Kind.HIGH_IS_S:     return "HIGH IS-S";
                case Kind.HIGH_IS_E:     return "HIGH IS-E";
                case Kind.HIGH_IS_E_S:   return "HIGH IS-E,S";
                case Kind.MUSTANG_IS_E:  return "MUSTANG IS-E";
                case Kind.N_PADRE_IS_E:  return "N PADRE IS-E";
                case Kind.S_PADRE_IS_E:  return "S PADRE IS-E";
                case Kind.SABINE_PASS:   return "SABINE PASS";
                default:                 throw new ArgumentOutOfRangeException(nameof(kind), kind, null);
            }
        }

        private static Kind FromName(string kind)
        {
            switch(kind)
            {
                case "None Selected": return Kind.None_Selected;
                case "ANDERSON":      return Kind.ANDERSON;
                case "ANDREWS":       return Kind.ANDREWS;
                case "ANGELINA":      return Kind.ANGELINA;
                case "ARANSAS":       return Kind.ARANSAS;
                case "ARCHER":        return Kind.ARCHER;
                case "ARMSTRONG":     return Kind.ARMSTRONG;
                case "ATASCOSA":      return Kind.ATASCOSA;
                case "AUSTIN":        return Kind.AUSTIN;
                case "BAILEY":        return Kind.BAILEY;
                case "BANDERA":       return Kind.BANDERA;
                case "BASTROP":       return Kind.BASTROP;
                case "BAYLOR":        return Kind.BAYLOR;
                case "BEE":           return Kind.BEE;
                case "BELL":          return Kind.BELL;
                case "BEXAR":         return Kind.BEXAR;
                case "BLANCO":        return Kind.BLANCO;
                case "BORDEN":        return Kind.BORDEN;
                case "BOSQUE":        return Kind.BOSQUE;
                case "BOWIE":         return Kind.BOWIE;
                case "BRAZORIA":      return Kind.BRAZORIA;
                case "BRAZOS":        return Kind.BRAZOS;
                case "BREWSTER":      return Kind.BREWSTER;
                case "BRISCOE":       return Kind.BRISCOE;
                case "BROOKS":        return Kind.BROOKS;
                case "BROWN":         return Kind.BROWN;
                case "BURLESON":      return Kind.BURLESON;
                case "BURNET":        return Kind.BURNET;
                case "CALDWELL":      return Kind.CALDWELL;
                case "CALHOUN":       return Kind.CALHOUN;
                case "CALLAHAN":      return Kind.CALLAHAN;
                case "CAMERON":       return Kind.CAMERON;
                case "CAMP":          return Kind.CAMP;
                case "CARSON":        return Kind.CARSON;
                case "CASS":          return Kind.CASS;
                case "CASTRO":        return Kind.CASTRO;
                case "CHAMBERS":      return Kind.CHAMBERS;
                case "CHEROKEE":      return Kind.CHEROKEE;
                case "CHILDRESS":     return Kind.CHILDRESS;
                case "CLAY":          return Kind.CLAY;
                case "COCHRAN":       return Kind.COCHRAN;
                case "COKE":          return Kind.COKE;
                case "COLEMAN":       return Kind.COLEMAN;
                case "COLLIN":        return Kind.COLLIN;
                case "COLLINGSWORTH": return Kind.COLLINGSWORTH;
                case "COLORADO":      return Kind.COLORADO;
                case "COMAL":         return Kind.COMAL;
                case "COMANCHE":      return Kind.COMANCHE;
                case "CONCHO":        return Kind.CONCHO;
                case "COOKE":         return Kind.COOKE;
                case "CORYELL":       return Kind.CORYELL;
                case "COTTLE":        return Kind.COTTLE;
                case "CRANE":         return Kind.CRANE;
                case "CROCKETT":      return Kind.CROCKETT;
                case "CROSBY":        return Kind.CROSBY;
                case "CULBERSON":     return Kind.CULBERSON;
                case "DALLAM":        return Kind.DALLAM;
                case "DALLAS":        return Kind.DALLAS;
                case "DAWSON":        return Kind.DAWSON;
                case "DEAF SMITH":    return Kind.DEAF_SMITH;
                case "DELTA":         return Kind.DELTA;
                case "DENTON":        return Kind.DENTON;
                case "DE WITT":       return Kind.DE_WITT;
                case "DICKENS":       return Kind.DICKENS;
                case "DIMMIT":        return Kind.DIMMIT;
                case "DONLEY":        return Kind.DONLEY;
                case "DUVAL":         return Kind.DUVAL;
                case "EASTLAND":      return Kind.EASTLAND;
                case "ECTOR":         return Kind.ECTOR;
                case "EDWARDS":       return Kind.EDWARDS;
                case "ELLIS":         return Kind.ELLIS;
                case "EL PASO":       return Kind.EL_PASO;
                case "ERATH":         return Kind.ERATH;
                case "FALLS":         return Kind.FALLS;
                case "FANNIN":        return Kind.FANNIN;
                case "FAYETTE":       return Kind.FAYETTE;
                case "FISHER":        return Kind.FISHER;
                case "FLOYD":         return Kind.FLOYD;
                case "FOARD":         return Kind.FOARD;
                case "FORT BEND":     return Kind.FORT_BEND;
                case "FRANKLIN":      return Kind.FRANKLIN;
                case "FREESTONE":     return Kind.FREESTONE;
                case "FRIO":          return Kind.FRIO;
                case "GAINES":        return Kind.GAINES;
                case "GALVESTON":     return Kind.GALVESTON;
                case "GARZA":         return Kind.GARZA;
                case "GILLESPIE":     return Kind.GILLESPIE;
                case "GLASSCOCK":     return Kind.GLASSCOCK;
                case "GOLIAD":        return Kind.GOLIAD;
                case "GONZALES":      return Kind.GONZALES;
                case "GRAY":          return Kind.GRAY;
                case "GRAYSON":       return Kind.GRAYSON;
                case "GREGG":         return Kind.GREGG;
                case "GRIMES":        return Kind.GRIMES;
                case "GUADALUPE":     return Kind.GUADALUPE;
                case "HALE":          return Kind.HALE;
                case "HALL":          return Kind.HALL;
                case "HAMILTON":      return Kind.HAMILTON;
                case "HANSFORD":      return Kind.HANSFORD;
                case "HARDEMAN":      return Kind.HARDEMAN;
                case "HARDIN":        return Kind.HARDIN;
                case "HARRIS":        return Kind.HARRIS;
                case "HARRISON":      return Kind.HARRISON;
                case "HARTLEY":       return Kind.HARTLEY;
                case "HASKELL":       return Kind.HASKELL;
                case "HAYS":          return Kind.HAYS;
                case "HEMPHILL":      return Kind.HEMPHILL;
                case "HENDERSON":     return Kind.HENDERSON;
                case "HIDALGO":       return Kind.HIDALGO;
                case "HILL":          return Kind.HILL;
                case "HOCKLEY":       return Kind.HOCKLEY;
                case "HOOD":          return Kind.HOOD;
                case "HOPKINS":       return Kind.HOPKINS;
                case "HOUSTON":       return Kind.HOUSTON;
                case "HOWARD":        return Kind.HOWARD;
                case "HUDSPETH":      return Kind.HUDSPETH;
                case "HUNT":          return Kind.HUNT;
                case "HUTCHINSON":    return Kind.HUTCHINSON;
                case "IRION":         return Kind.IRION;
                case "JACK":          return Kind.JACK;
                case "JACKSON":       return Kind.JACKSON;
                case "JASPER":        return Kind.JASPER;
                case "JEFF DAVIS":    return Kind.JEFF_DAVIS;
                case "JEFFERSON":     return Kind.JEFFERSON;
                case "JIM HOGG":      return Kind.JIM_HOGG;
                case "JIM WELLS":     return Kind.JIM_WELLS;
                case "JOHNSON":       return Kind.JOHNSON;
                case "JONES":         return Kind.JONES;
                case "KARNES":        return Kind.KARNES;
                case "KAUFMAN":       return Kind.KAUFMAN;
                case "KENDALL":       return Kind.KENDALL;
                case "KENEDY":        return Kind.KENEDY;
                case "KENT":          return Kind.KENT;
                case "KERR":          return Kind.KERR;
                case "KIMBLE":        return Kind.KIMBLE;
                case "KING":          return Kind.KING;
                case "KINNEY":        return Kind.KINNEY;
                case "KLEBERG":       return Kind.KLEBERG;
                case "KNOX":          return Kind.KNOX;
                case "LAMAR":         return Kind.LAMAR;
                case "LAMB":          return Kind.LAMB;
                case "LAMPASAS":      return Kind.LAMPASAS;
                case "LA SALLE":      return Kind.LA_SALLE;
                case "LAVACA":        return Kind.LAVACA;
                case "LEE":           return Kind.LEE;
                case "LEON":          return Kind.LEON;
                case "LIBERTY":       return Kind.LIBERTY;
                case "LIMESTONE":     return Kind.LIMESTONE;
                case "LIPSCOMB":      return Kind.LIPSCOMB;
                case "LIVE OAK":      return Kind.LIVE_OAK;
                case "LLANO":         return Kind.LLANO;
                case "LOVING":        return Kind.LOVING;
                case "LUBBOCK":       return Kind.LUBBOCK;
                case "LYNN":          return Kind.LYNN;
                case "MCCULLOCH":     return Kind.MCCULLOCH;
                case "MCLENNAN":      return Kind.MCLENNAN;
                case "MCMULLEN":      return Kind.MCMULLEN;
                case "MADISON":       return Kind.MADISON;
                case "MARION":        return Kind.MARION;
                case "MARTIN":        return Kind.MARTIN;
                case "MASON":         return Kind.MASON;
                case "MATAGORDA":     return Kind.MATAGORDA;
                case "MAVERICK":      return Kind.MAVERICK;
                case "MEDINA":        return Kind.MEDINA;
                case "MENARD":        return Kind.MENARD;
                case "MIDLAND":       return Kind.MIDLAND;
                case "MILAM":         return Kind.MILAM;
                case "MILLS":         return Kind.MILLS;
                case "MITCHELL":      return Kind.MITCHELL;
                case "MONTAGUE":      return Kind.MONTAGUE;
                case "MONTGOMERY":    return Kind.MONTGOMERY;
                case "MOORE":         return Kind.MOORE;
                case "MORRIS":        return Kind.MORRIS;
                case "MOTLEY":        return Kind.MOTLEY;
                case "NACOGDOCHES":   return Kind.NACOGDOCHES;
                case "NAVARRO":       return Kind.NAVARRO;
                case "NEWTON":        return Kind.NEWTON;
                case "NOLAN":         return Kind.NOLAN;
                case "NUECES":        return Kind.NUECES;
                case "OCHILTREE":     return Kind.OCHILTREE;
                case "OLDHAM":        return Kind.OLDHAM;
                case "ORANGE":        return Kind.ORANGE;
                case "PALO PINTO":    return Kind.PALO_PINTO;
                case "PANOLA":        return Kind.PANOLA;
                case "PARKER":        return Kind.PARKER;
                case "PARMER":        return Kind.PARMER;
                case "PECOS":         return Kind.PECOS;
                case "POLK":          return Kind.POLK;
                case "POTTER":        return Kind.POTTER;
                case "PRESIDIO":      return Kind.PRESIDIO;
                case "RAINS":         return Kind.RAINS;
                case "RANDALL":       return Kind.RANDALL;
                case "REAGAN":        return Kind.REAGAN;
                case "REAL":          return Kind.REAL;
                case "RED RIVER":     return Kind.RED_RIVER;
                case "REEVES":        return Kind.REEVES;
                case "REFUGIO":       return Kind.REFUGIO;
                case "ROBERTS":       return Kind.ROBERTS;
                case "ROBERTSON":     return Kind.ROBERTSON;
                case "ROCKWALL":      return Kind.ROCKWALL;
                case "RUNNELS":       return Kind.RUNNELS;
                case "RUSK":          return Kind.RUSK;
                case "SABINE":        return Kind.SABINE;
                case "SAN AUGUSTINE": return Kind.SAN_AUGUSTINE;
                case "SAN JACINTO":   return Kind.SAN_JACINTO;
                case "SAN PATRICIO":  return Kind.SAN_PATRICIO;
                case "SAN SABA":      return Kind.SAN_SABA;
                case "SCHLEICHER":    return Kind.SCHLEICHER;
                case "SCURRY":        return Kind.SCURRY;
                case "SHACKELFORD":   return Kind.SHACKELFORD;
                case "SHELBY":        return Kind.SHELBY;
                case "SHERMAN":       return Kind.SHERMAN;
                case "SMITH":         return Kind.SMITH;
                case "SOMERVELL":     return Kind.SOMERVELL;
                case "STARR":         return Kind.STARR;
                case "STEPHENS":      return Kind.STEPHENS;
                case "STERLING":      return Kind.STERLING;
                case "STONEWALL":     return Kind.STONEWALL;
                case "SUTTON":        return Kind.SUTTON;
                case "SWISHER":       return Kind.SWISHER;
                case "TARRANT":       return Kind.TARRANT;
                case "TAYLOR":        return Kind.TAYLOR;
                case "TERRELL":       return Kind.TERRELL;
                case "TERRY":         return Kind.TERRY;
                case "THROCKMORTON":  return Kind.THROCKMORTON;
                case "TITUS":         return Kind.TITUS;
                case "TOM GREEN":     return Kind.TOM_GREEN;
                case "TRAVIS":        return Kind.TRAVIS;
                case "TRINITY":       return Kind.TRINITY;
                case "TYLER":         return Kind.TYLER;
                case "UPSHUR":        return Kind.UPSHUR;
                case "UPTON":         return Kind.UPTON;
                case "UVALDE":        return Kind.UVALDE;
                case "VAL VERDE":     return Kind.VAL_VERDE;
                case "VAN ZANDT":     return Kind.VAN_ZANDT;
                case "VICTORIA":      return Kind.VICTORIA;
                case "WALKER":        return Kind.WALKER;
                case "WALLER":        return Kind.WALLER;
                case "WARD":          return Kind.WARD;
                case "WASHINGTON":    return Kind.WASHINGTON;
                case "WEBB":          return Kind.WEBB;
                case "WHARTON":       return Kind.WHARTON;
                case "WHEELER":       return Kind.WHEELER;
                case "WICHITA":       return Kind.WICHITA;
                case "WILBARGER":     return Kind.WILBARGER;
                case "WILLACY":       return Kind.WILLACY;
                case "WILLIAMSON":    return Kind.WILLIAMSON;
                case "WILSON":        return Kind.WILSON;
                case "WINKLER":       return Kind.WINKLER;
                case "WISE":          return Kind.WISE;
                case "WOOD":          return Kind.WOOD;
                case "YOAKUM":        return Kind.YOAKUM;
                case "YOUNG":         return Kind.YOUNG;
                case "ZAPATA":        return Kind.ZAPATA;
                case "ZAVALA":        return Kind.ZAVALA;
                case "S PADRE IS-SB": return Kind.S_PADRE_IS_SB;
                case "N PADRE IS-SB": return Kind.N_PADRE_IS_SB;
                case "MUSTANG IS-SB": return Kind.MUSTANG_IS_SB;
                case "MATGRDA IS-SB": return Kind.MATGRDA_IS_SB;
                case "BRAZOS-SB":     return Kind.BRAZOS_SB;
                case "GALVESTON-SB":  return Kind.GALVESTON_SB;
                case "HIGH IS-SB":    return Kind.HIGH_IS_SB;
                case "S PADRE IS-LB": return Kind.S_PADRE_IS_LB;
                case "N PADRE IS-LB": return Kind.N_PADRE_IS_LB;
                case "MUSTANG IS-LB": return Kind.MUSTANG_IS_LB;
                case "MATGRDA IS-LB": return Kind.MATGRDA_IS_LB;
                case "BRAZOS-LB":     return Kind.BRAZOS_LB;
                case "BRAZOS-S":      return Kind.BRAZOS_S;
                case "GALVESTON-LB":  return Kind.GALVESTON_LB;
                case "GALVESTON-S":   return Kind.GALVESTON_S;
                case "HIGH IS-LB":    return Kind.HIGH_IS_LB;
                case "HIGH IS-S":     return Kind.HIGH_IS_S;
                case "HIGH IS-E":     return Kind.HIGH_IS_E;
                case "HIGH IS-E,S":   return Kind.HIGH_IS_E_S;
                case "MUSTANG IS-E":  return Kind.MUSTANG_IS_E;
                case "N PADRE IS-E":  return Kind.N_PADRE_IS_E;
                case "S PADRE IS-E":  return Kind.S_PADRE_IS_E;
                case "SABINE PASS":   return Kind.SABINE_PASS;

                default: throw new ArgumentOutOfRangeException(nameof(kind), kind, null);
            }
        }

        public bool Equals(CountyType other)
        {
            if(ReferenceEquals(null, other))
            {
                return false;
            }

            if(ReferenceEquals(this, other))
            {
                return true;
            }

            return Value == other.Value;
        }

        public bool Equals(Kind other)
        {
            return Value == other;
        }

        public override bool Equals(object obj)
        {
            return ReferenceEquals(this, obj) || obj is CountyType other && Equals(other);
        }

        public override int GetHashCode()
        {
            return Value.GetHashCode();
        }

        public static bool operator ==(CountyType left,
                                       CountyType right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(CountyType left,
                                       CountyType right)
        {
            return !Equals(left, right);
        }

        public static bool operator ==(Kind       left,
                                       CountyType right)
        {
            return right != null && left == right.Value;
        }

        public static bool operator !=(Kind       left,
                                       CountyType right)
        {
            return !(left == right);
        }

        public static bool operator ==(CountyType left,
                                       Kind       right)
        {
            return left != null && left.Value == right;
        }

        public static bool operator !=(CountyType left,
                                       Kind       right)
        {
            return !(left == right);
        }
    }
}
