namespace OilGas.Data.RRC.Texas
{
    public enum OilTape
    {
        ///Well Root Segment (Oil version)
        WLROOT = 1,

        ///Oil Reporting Cycle Segment 
        WLORPTCY = 2,

        ///Oil Previous Allowable Segment 
        WLOPRVAL = 3,

        ///Oil Forms Lacking Segment 
        WLOFRMLK = 4,

        ///Oil Forms Lacking Description Segment
        WLOFRMDS = 5,

        ///East Texas Segment  
        WLOETEX = 6,

        ///Designated Casing Leakage Well Segment
        WLOETDS = 7,

        ///Oil Allowable Transfers Segment 
        WLOALTF = 8,

        ///Oil Transferred Wells Segment 
        WLOTFWL = 9,

        ///Panhandle Gas Production Segment 
        WLPHPROD = 10,

        ///Previous Oil Well Types Segment
        WLPVTYP = 11,

        ///Panhandle Gas Balancing Segment 
        WLPHBAL = 12,

        ///Form W-10 Segment  
        WLOW10 = 13,

        ///Form W-10 Previous Segment 
        WLOPVW10 = 14,

        ///Well Remark Segment  
        WLREMARK = 15,

        ///Sealed Well Segment  
        WLSEAL = 16,

        ///Oil Schedule/UIC Segment  
        WLOUIC = 26,

        ///Senate Bill 126 (2-Yr Inactive) Segment
        WLSB126 = 28
    }
}
