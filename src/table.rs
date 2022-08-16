use std::fmt::{Debug, Formatter};
use crate::object::ObjString;
use crate::{As, Value};

const TABLE_MAX_LOAD: f32 = 0.75;
pub struct Table {
    count : usize,
    capacity : usize,
    entries : Vec<Entry>
}

#[derive(Debug,Clone)]
pub struct Entry {
    key : ObjString,
    value: Value
}

impl Entry {
    fn empty() -> Self {
        Self {
            key: ObjString::empty(),
            value: Value::empty()
        }

    }

    pub fn free(&mut self) {
        self.key.free();
        self.value.free();
    }
}
impl Debug for Table {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();
        for entry in &self.entries {
            string.push_str(&format!("{:?}",entry));
            string.push_str("\n");
        }

        f.write_str(&string)
    }
}
impl Table {
    pub fn new() -> Self {
        Self {
            count:0 ,
            capacity:0,
            entries: vec![]
        }
    }

    pub fn free(&mut self) {
        self.capacity = 0;
        self.count = 0;
        for entry in self.entries.iter_mut(){
            entry.free()
        }
        self.entries = vec![];

    }

    pub fn set(&mut self, key : ObjString, value : Value) -> bool {

        /**
          if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
          int capacity = GROW_CAPACITY(table->capacity);
          adjustCapacity(table, capacity);
        }
        */
        /**
           Entry* entry = findEntry(table->entries, table->capacity, key);
        bool isNewKey = entry->key == NULL;
        if (isNewKey) table->count++;

        entry->key = key;
        entry->value = value;
        return isNewKey;
         */

        if ((self.count + 1) as f32 > (self.capacity as f32) * TABLE_MAX_LOAD) {
            // we need to grow our array to
            let capacity = self.grow_capacity();
            println!("before adjusting capacity : \n{:?}",&self);
            println!("*********************************************************");
            self.adjust_capacity(capacity);
            println!("after adjusting capacity : \n{:?}",&self);
        }

        let mut entry = Self::find_entry(&mut self.entries ,self.capacity, &key);
        let isNewKey = match (*entry).value.rep {
            As::Empty => true,
            _ => false
        };
        (*entry).key = key;
        (*entry).value = value;


        if (isNewKey) {
            self.count += 1;
        }
        isNewKey


    }

    fn grow_capacity(&self) -> usize {
        let updated_capacity = if(self.capacity <8){ 8 } else {self.capacity * 2};
        // self.entries.resize_with(updated_capacity, || Entry::empty());
        // self.capacity = updated_capacity;
        updated_capacity
    }

    fn adjust_capacity(&mut self,capacity : usize) {
        /**
         Entry* entries = ALLOCATE(Entry, capacity);
        for (int i = 0; i < capacity; i++) {
          entries[i].key = NULL;
          entries[i].value = NIL_VAL;
        }

        table->entries = entries;
        table->capacity = capacity;
         */


        /**
         for (int i = 0; i < table->capacity; i++) {
          Entry* entry = &table->entries[i];
          if (entry->key == NULL) continue;

          Entry* dest = findEntry(entries, capacity, entry->key);
          dest->key = entry->key;
          dest->value = entry->value;
        }
         */

        // We want to move from a smaller to a bigger hash map

        // first we create the bigger hash map with empty values
        let mut updated_entries = vec![Entry::empty(); capacity]; // we have resized the array of entries

        // now for every entry in the previous map (smaller map)
        // if the entry is empty, then there's nothing to do, as the destination (bigger) map is all filled with empty values
        // if the entry is not empty, then we find the entry/bucket the key is supposed to be in the bigger map.
        // this is important because due to collision, an entry could be elsewhere
        /**
         As an example, if the initial map size was 8, and we want to store an entry with a hash value of 22,
        it will be in bucket 6 (22 % 8), now if we want to add another entry with hash of 30, its bucket is 6 also and that causes a collision,
        which 'may' result in the entry going to bucket 7 instead.

        Now, when we create a bigger map (16 buckets), there's more space and as such, now if we find the buckets for hash 22 and 30, they don't collide
        anymore, they go into buckets 6 and 14 respectively.
        That's why when copying over elements from the smaller map to the bigger map, we need to find the appropriate bucket/ entry for each element from the smaller map

         */
        for entry in &self.entries {
           if(entry.key.is_empty()) {
               // nothing to do
               continue
           } else {
               // find the destination in the new/updated/bigger map and store the entry from the smaller map there instead
               let destination = Self::find_entry(&mut updated_entries,capacity,&entry.key);
               destination.key = entry.key;
               destination.value = entry.value

           }
        }

        self.entries = updated_entries;
        self.capacity = capacity;
    }
    fn find_entry<'a>(entries : &'a mut Vec<Entry>, capacity : usize ,key : &ObjString) -> &'a mut Entry {

        /**
         static Entry* findEntry(Entry* entries, int capacity,
                        ObjString* key) {
          uint32_t index = key->hash % capacity;
          for (;;) {
            Entry* entry = &entries[index];
            if (entry->key == key || entry->key == NULL) {
              return entry;
            }

            index = (index + 1) % capacity;
          }
        }
         */

        let mut index = (key.hash as usize % capacity) as usize;
        unsafe {
            loop {
                let mut entry  = (entries.get(index).expect("index out of bounds"));
                match (*entry).key {
                    p @ObjString { .. } =>  {
                        if(p == *key || (*entry).value.is_empty()) {
                            return entries.get_mut(index).expect("index out of bounds");
                        }
                    }
                }
                index = (index + 1) % capacity;
            }
        }
    }

    fn add_all(from : &mut Table, to : &mut Table) {
        /**
        void tableAddAll(Table* from, Table* to) {
          for (int i = 0; i < from->capacity; i++) {
            Entry* entry = &from->entries[i];
            if (entry->key != NULL) {
              tableSet(to, entry->key, entry->value);
            }
          }
        }
        */

        for from_entry in &from.entries {
            if(!from_entry.key.is_empty()) {
                to.set(from_entry.key, from_entry.value);
            }
        }


    }
}


#[cfg(test)]
mod tests {
    use crate::object::Obj;
    use super::*;
    #[test]
    fn test_table(){

        let mut map = Table::new();

        map.set(ObjString::from_buffer("hello1".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello2".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello3".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello4".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello5".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello6".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello7".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello8".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello9".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello10".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello11".as_bytes()),Value::bool_value(true));
        map.set(ObjString::from_buffer("hello12".as_bytes()),Value::bool_value(true));

       //println!("{:?}",map);

    }
}