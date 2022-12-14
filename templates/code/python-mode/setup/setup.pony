# -*- mode: snippet -*-
# name: pony-setup
# uuid: pony-setup
# key:  pony-setup
# group : setup
# --

##-- imports
import pony.orm as pony
##-- end imports

##-- debugging
pony.set_sql_debug(True)
##-- end debugging

##-- initiatilization
db = pony.Database()
##-- end initiatilization

##-- data mapping
class Person(db.Entity):
    name : str = pony.Required(str)
    age  : int = pony.Required(int)
    cars : set = pony.Set('Car')

class Car(db.Entity):
    make  : str = pony.Required(str)
    model : str = pony.Required(str)
    owner : Person = pony.Required(Person)


db.bind(provider='sqlite', filename="test_db.sqlite", create_db=True)
db.generate_mapping(create_tables=True)
##-- end data mapping


##-- session use
with pony.db_session:
    p1 = Person(name='John', age=20)
    p2 = Person(name='Mary', age=22)
    p3 = Person(name='Bob', age=30)
    c1 = Car(make='Toyota', model='Prius', owner=p2)
    c2 = Car(make='Ford', model='Explorer', owner=p3)
    pony.commit()

    query = pony.select(p for p in Person if p.age > 20)
    result = query[:]
    breakpoint()
##-- end session use
