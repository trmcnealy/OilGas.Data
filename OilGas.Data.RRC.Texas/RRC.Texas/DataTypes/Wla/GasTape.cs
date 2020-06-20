namespace OilGas.Data.RRC.Texas
{
    public enum GasTape
    {
        ///Well Root Segment (Gas version) 
        WLROOT = 1,

        ///Well Remark Segment   
        WLREMARK = 15,

        ///Sealed Well Segment   
        WLSEAL = 16,

        ///Form G-1 Segment   
        WLGG1 = 17,

        ///Form G-10 Segment   
        WLGG10 = 18,

        ///Form G-10 Previous Segment  
        WLGPVG10 = 19,

        ///Form GC-1 Segment   
        WLGC1 = 20,

        ///Gas Reporting Cycle Segment  
        WLGRPTCY = 21,

        ///Gas Forms Lacking Segment  
        WLGFRMLK = 22,

        ///Gas Forms Lacking Description Segment 
        WLGFRMDS = 23,

        ///Gas Allowable Transfers Segment  
        WLGALTF = 24,

        ///Gas Previous Allowable Segment  
        WLGPRVAL = 25,

        ///Gas Schedule/UIC Segment   
        WLGUIC = 27,

        ///Senate Bill 126 (2-Yr Inactive) Segment
        WLSB126 = 28
    }
}
