using System;

namespace OilGas.Data.RRC.Texas
{
    public ref struct RecordLayoutTemplate
    {
        public ReadOnlySpan<byte> Key;

        public ReadOnlySpan<byte> Record;

        public RecordLayoutTemplate(ReadOnlySpan<byte> record)
        {
            Key    = record.Slice(0, 2);
            Record = record;
        }

        public short GetKeyValue()
        {
            return short.Parse(Utilities.ToAscii(Key));
        }
    }
}
