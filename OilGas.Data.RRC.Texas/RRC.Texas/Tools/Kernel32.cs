using System;
using System.Runtime.InteropServices;

namespace OilGas.Data.RRC.Texas
{
    internal static class Kernel32
    {
        internal const uint LMEM_FIXED    = 0x0000;
        internal const uint LMEM_MOVEABLE = 0x0002;

        [DllImport("kernel32")]
        internal static extern IntPtr LocalAlloc(uint  uFlags,
                                                 ulong uBytes);

        [DllImport("kernel32")]
        internal static extern IntPtr LocalReAlloc(IntPtr hMem,
                                                   ulong  uBytes,
                                                   uint   uFlags);

        [DllImport("kernel32", SetLastError = true)]
        internal static extern IntPtr LocalFree(IntPtr hMem);
    }
}
