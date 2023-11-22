# Setting vaiables

foo = 1;
bar = "hello";
bat = 100;
mylist = [1, 13, 1.5, -8];
textlist = [mylist,0,"Thanksgiving",bar,bat];
fullname = "Yun"+" Shi";
fullname;
list0 = [1];
list0 # list: a different structure than a vector (R)
list0[0];
c1=[1,5,3];
c2=[9,-1,1.8];
c3=c1+c2; # use + to combine list
c3;

# example of a dictonary
dictonary1={'var_a' : 3, 'var_b': 15, 'var_c' : 0}; #var_a called keys, 3 is called values
dictonary1['var_b'];
dictonary1.get('var_b')
zoo=dictonary1.get('var_x')
# if the variable does not exist, it will create a "None" variable in python, like "NULL" in R
zoo
type(zoo) #<class 'NoneType'>, type is similar to class function in R
dictonary1['var_y']; # error message
dictonary2={'var_a' : 'Happy', 'var_b': 'Thanksgiving'};
dictonary2['var_a'+'var_b'];
dictonary2;
dictonary1.keys();
dictonary1.values();
dictonary1.items();

#Dictonary comprehension
{kk:vv for kk,vv in dictonary1.items()};
{kk:vv for kk,vv in dictonary1.items() if kk in ['var_a','var_c']};

#List comprehension
[vv for kk,vv in dictonary1.items() if kk in ['var_a','var_c']];
[kk for kk,vv in dictonary1.items() if vv < 10];
# where kk:vv can be used for any function, similar like lapply pass it onto first variable

[vv for vv in textlist];
[vv for vv in textlist if isinstance(vv,int)];
[vv for vv in textlist if isinstance(vv,list)];

#Commonly used types: str, int, float

"""list vs vector, different in R
in python, "dictionary" is  a collection of values of variable types and can only be referrenced by names
list can be any type, can be referenced by location. list can contain other list in python"""
