using System;

namespace OilGas.Data
{
    public sealed class DataTableAttribute : Attribute
    {
        public string Name { get; set; }

        public DataTableAttribute(string name)
        {
            Name = name;
        }

        private DataTableAttribute()
        {
        }
    }
}