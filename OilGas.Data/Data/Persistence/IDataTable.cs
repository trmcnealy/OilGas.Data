using System;

namespace OilGas.Data
{
    public interface IDataTable<T>
    {
        public T Id { get; set; }
    }
}