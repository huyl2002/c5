/*
    网友Robert Swierczek写了500行的C语言编译器C4 https://github.com/rswier/c4, 用4个函数实现编译器（C4名称的由来)
	本项目是C4的简化，为降低难度，不再自举。
	在VC08@WIN10，Ubuntu10.04上测试通过。
	Author huyl2002, huyl2002@sina.com  2017.10.06
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h> // va_list
#include <fcntl.h>

#ifndef __cplusplus
    typedef int bool;
    #define true 1
    #define false 0
#endif

/* Token集合，运算符按优先级从小到大排列
   其中赋值操作，看做二元操作符
*/
enum Token
{
	Num = 128, Loc, Fun, Sys,
	Id,
	Char, Int, If, Else, While, Return,
	Assign,
	Eq, Ne, Gt,
	Add, Sub, Mul, Div
};

/* 汇编指令
依次为  14个基本操作
          3 个条件指令(本质是二元运算指令)
		  4个运算指令
		  2个系统调用（printf和exit）

ENT 在一个函数头部执行，保护现场
LEV 在一个函数尾部执行，恢复现场，恢复pc（暗含跳转操作）
JSR 实现对一个函数的调用，（先将pc压栈），JSR执行之前，caller将函数参数压栈，
ADJ 一般在JSR后调用，弹出函数参数
*/
enum Code
{
	ENT = 1, LEV, JSR, ADJ,
	JMP, BZ,  BNZ,
	LEA, IMM, PSH, LI,  LC,  SI,  SC,

	EQ, NE, GT,
	ADD, SUB, MUL, DIV,
	PRT, EXT
};

enum VarType
{
	CHAR, INT, PTR
};

#define PoolLen (256*1024)

// 符号表（描述标识符的数据结构）
typedef struct _Identifier
{
	int tk;   // 例如Id, Char
	int hash; // 用于比较两个Identifier是否相同
	int name; // 指向该识别符avatar地址(在bss中)
	int kind; // Glo, Fun, Sys, Num(仅全局enum变量)等
	int type; // CHAR, INT, PTR等

	int val; /* 当全局枚举时，它的值就是所代表值
                当全局变量时，id[Val] = (int)data; 即此变量的地址
                当为函数参数时，（解析函数体时），将该值id[Val] = 0,1,2(按参数顺序)
                当为函数名时，它记录  id[Val] = (int)(e + 1); // 即ENT命令所在地址
                当为系统调用时，id[Val] = OPEN +i, 即printf对应指令为PRTF
				当为局部变量时，第一个局部变量为loc + 1, 第二个为loc + 2，以此类推
                if (d[Class] == Sys) *++e = d[Val];
		 */
	int hkind; // hide kind
	int htype; // hide type
	int hval;
} Identifier;

Identifier table[1000]; // 符号表
// 用于词法解析
char* p;
char* lp;
int tk;
int id;
int idmain;
int nVal;
char* data;
int line;
int col;

// 用于语义解析
int loc;  // 值为函数参数个数加1
int ty;   // 仅供expr使用, 赋值语句时，它为左操作数的类型;
          // 函数调用时，它是返回值类型; 为字符串时，它为PTR
int* e;
int* le;
int text[PoolLen]; // 指令区 (解析结果)
char bss[PoolLen]; // 数据区（解析结果，存储全局变量和字符串）

// 虚拟机的栈
int stack[PoolLen + 1];

bool parse(char* src);
bool run(int* startaddr);
bool panic(const char* file, const char* func, int c5line, int row, int col, const char* format, ...);
void _check(bool flag, const char* file, const char* func, int c5line, int row, int col);

#define Panic(format, ...) panic(__FILE__, __FUNCTION__, __LINE__, line, col, format, ##__VA_ARGS__)
#define Wrong  Panic("")
#define check(x) _check((bool)x, __FILE__, __FUNCTION__, __LINE__, line, col)
#define match(x) _check((tk==x), __FILE__, __FUNCTION__, __LINE__, line, col)

/* 建立四个测试用例a0 - a3，调用函数parse，生成汇编代码（存储在全局变量text中）
   调用函数run()执行汇编代码。
*/
int main(int argc, char** argv)
{
	bool r;
	int* addr; // 入口地址
	char* a0 = " #include <stdio.h>                         \n"
		       " int main()                                 \n"
			   " {                                          \n"
			   "    int var1;                               \n"
			   "    var1 = 3;                               \n"
			   "    printf(\"var1:%d \n\", var1);           \n"
			   " }                                          \n";
	char* a1 = " #include <stdio.h>                         \n"
		       " int main()                                 \n"
			   " {                                          \n"
			   "    int var1;                               \n"
			   "    var1 = 3*5/7 + 10;                      \n"
			   "    printf(\"var1:%d \n\", var1);           \n"
			   " }                                          \n";
	char* a2 = " #include <stdio.h>                         \n"
			   " int main()                                 \n"
			   " {                                          \n"
			   "    int var1;                               \n"
			   "    var1 = 19;                              \n"
			   "    while(var1 > 10)                        \n"
			   "    {                                       \n"
			   "        var1 = var1 - 2;                    \n"
			   "    }                                       \n"
			   "    printf(\"var1:%d \n\", var1);           \n"
			   " }                                          \n";
	char* a3 = " #include <stdio.h>                         \n"
		       " int sum(int para1, int para2)              \n"
			   " {                                          \n"
			   "     return para1 + para2;                  \n"
			   " }                                          \n"
			   "                                            \n"
			   " int main()                                 \n"
			   " {                                          \n"
			   "    int var1, var2, var3;                   \n"
			   "    var1 = 19;                              \n"
			   "    var2 = 20;                              \n"
			   "    var3 = sum(var1, var2);                 \n"
			   "    printf(\"var3:%d \n\", var3);           \n"
			   " }                                          \n";
	if(0) r = parse(a0);
	if(0) r = parse(a1);
	if(0) r = parse(a2);
	if(1) r = parse(a3);
	check(r);
	addr = (int*)table[idmain].val;
	check(addr);
    run(addr);
	getchar();
	return 0;
}

bool isNumber(int c) {  return (c >= '0' && c <='9'); }
bool isAlpha(int c) { return ((c>='a') && (c<='z')) || ((c>='A') && (c<='Z')) || c=='_'; }

// 打印一行C代码及其对应的汇编代码
void printCode()
{
	char* codes = "ENT, LEV, JSR, ADJ, JMP, BZ , BNZ, LEA, IMM, PSH, LI , LC , "
		          "SI , SC , EQ , NE , GT , ADD, SUB, MUL, DIV, PRT, EXT, ";
	printf("%d: %.*s", line, p-lp, lp);
	lp = p;
	while(le < e)
	{
		printf("%8.3s", &codes[*++le * 5 - 5]);
		if(*le <= IMM && *le != LEV)
			printf("  %d", *++le);
		printf("\n");
	}
}

/* 词法分析：从字符串p中获取下一个TK
 全局变量tk有多种用途，其中之一是在这里做内部临时变量，
 例如Token为ID的情况下，tk作为临时变量用来计算hash值，最终tk被复制Id
next修改5个全局变量：p, tk, nVal，data, id
乘法操作符*的tk为Mul而非‘*’，是为方便优先级排列。运算符的Token按优先级从小到大排列
*/
void next()
{
	char* p0;
	col++;
	while(*p)
	{
		tk = *p++;
		if(tk == '\n')     {  printCode(); line++; col = 0;}
		else if(tk == '#') {  while(*p !=0 && *p != '\n') p++; } // 忽略include和define等预处理语句
		else if(isAlpha(tk)) // 函数、变量、系统调用和保留字
		{
			p0 = p - 1;
			while(isAlpha(*p) || isNumber(*p))
				tk = tk*147 + *p++; // 计算hash值
			tk = (tk << 6) + (p - p0);  id = 0;
			while(table[id].tk) // 遍历table
			{
				if(tk == table[id].hash && !memcmp((char*)table[id].name, p0, p - p0))
				{  tk = table[id].tk; return; }
				id++;
			}
			table[id].name = (int)p0; table[id].hash = tk;
			tk = table[id].tk = Id;
			return;
		}
		else if(isNumber(tk)) // 数字存在nVal中
		{
			nVal = tk - '0'; check(nVal);
			while(isNumber(*p)) nVal = nVal*10 + (*p++ - '0');
			tk = Num; return;
		}
		else if(tk == '/') // 注释或除法
		{
			if(*p == '/') { p++; while(*p != 0 && *p != '\n') p++; } // 这里不return，继续查找下一个token
			else { tk = Div; return; } // 除法
		}
		else if(tk == '"') // 字符串的tk是'"'，值放在data中,地址放在nVal
		{
			p0 = data;
			while(*p != 0 && *p != tk)
			{
				nVal = *p++;
				if(nVal == '\\') // 转义符 '\n' 认为是'\n' 其他直接忽略'\'转义
				{
					nVal = *p++;
					if(nVal == 'n') nVal = '\n';
				}
				*data++ = nVal;
			}
			p++; nVal = (int)p0; return;
		}
		else if(tk == '=')  { if(*p == '=') { p++; tk = Eq; } else tk = Assign; return; }
		else if(tk == '+')  { if(*p == '+') { p++; Wrong; } tk = Add; return; }
		else if(tk == '-')  { if(*p == '-') { p++; Wrong; } tk = Sub; return; }
		else if(tk == '*')  { tk = Mul; return;}
		else if (tk == '>') { check(*p != '=' && *p != '>'); tk = Gt; return; }
		else if (tk=='~' || tk==';' || tk=='{' || tk=='}' || tk=='(' || tk ==')' || tk == ']' || tk == ',' || tk == ':') return; // 直接返回
		else if (tk=='\''|| tk=='!' || tk=='<' || tk=='|' || tk=='&' || tk=='^' || tk=='%' || tk == '[' || tk == '?')  Wrong;
		else continue; // 这里处理空格等特殊字符
	} // while(*p)
	tk = *p;
	return;
}

/* 表达式解析函数
   expr被外部调用4次，都是stmt，参数皆为Assign，expr()再次调用expr()时，lev必定增加
   expr退出的条件：当前tk优先级低于level
   优先级最低的操作符是赋值。
   expr处理对象：表达式（不含大括号），不处理分号。
   表达式式操作数和操作符的组合。
   expr返回时，AX中放epxr结果，tk指向下一个低级运算符(或者分号、‘}’等)
   expr操作涉及两个栈，一个是可见的存储区stack，存储变量，另一个是expr函数自身的栈，存储操作符。
   后半部分，每次循环，前进一个操作符
*/
void expr(int lev)
{
	int d, t;
	check(tk);
	if(tk == Id)
	{
		d = id; next();  // printEnv();
		if(tk != '(') // 局部变量
		{
			check(table[d].kind == Loc);
			*++e = LEA; *++e = loc - table[d].val;
			*++e = ((ty=table[d].type)==CHAR) ? LC : LI; // 变量（地址已存在A）读取到A
		}
		else // 函数（系统）调用
		{
			next(); t=0;
			while(tk !=')') { expr(Assign); *++e = PSH; ++t;  if(tk==',') next(); } // 逐个解析实参
			next();
			if(table[d].kind == Fun)  { *++e = JSR; *++e = table[d].val; }
			else if(table[d].kind == Sys) { *++e = table[d].val; }
			else Wrong;
			if(t) { *++e = ADJ; *++e = t; }
			ty = table[d].type;
		}
	}
	else if(tk == Num) { *++e = IMM; *++e = nVal; next(); ty = INT; }
	else if(tk == '"') // 字符串
	{
		*++e = IMM; *++e = nVal; next();
		while(tk=='"') next(); // "C5""IS""A""COMPILER"解析成一个字符串
		data=(char*)((int)data + (sizeof(int) & -(int)sizeof(int))); ty = PTR; // 地址对齐
	}
	else if(tk == Add) { next(); expr(Div+1); ty = INT; } // 正号
	else if(tk == Sub) // 负号
	{
		next(); *++e = IMM;
		if(tk == Num) { *++e = -nVal; next(); }
		else { *++e = -1; *++e = PSH; expr(Div+1); *++e = MUL; } // 形如-(expr)的语句
		ty = INT;
	}
	else if(tk == Mul) // 取指
	{
		next(); expr(Div+1); check(ty > INT); ty -= PTR;
		*++e = (ty == CHAR) ? LC : LI;
	}
	else if(tk == '(')
	{
		next(); check(tk != Int && tk != Char);
		expr(Assign); match(')'); next();
	}
	else Wrong;
    // ------- 后半部分 -----------
	while(tk >= lev)
	{
		t = ty;
		if(tk == Assign)
		{
			next(); check(*e == LC || *e == LI); *e = PSH; // 将操作数地址（存在A）压栈
			expr(Assign);  *++e = ((ty=t) == CHAR) ? SC : SI; // SC SI 看做赋值运算符的实现
		}
		else if(tk == Eq)  { next(); *++e = PSH; expr(Gt);  *++e = EQ; ty = INT; }
		else if(tk == Ne)  { next(); *++e = PSH; expr(Gt);  *++e = NE; ty = INT; }
		else if(tk == Gt)  { next(); *++e = PSH; expr(Add); *++e = GT; ty = INT; }
		else if(tk == Add) { next(); *++e = PSH; expr(Mul); *++e = ADD; }
		else if(tk == Sub) { next(); *++e=PSH; expr(Mul); *++e = SUB; }
		else if(tk == Mul) { next(); *++e = PSH; expr(Div+1); *++e= MUL; ty = INT; }
		else if(tk == Div) { next(); *++e = PSH; expr(Div+1); *++e = DIV; ty = INT; }
		else Wrong;
	} // while(tk >= lev)
}

/* 语义块解析函数
   例如
   if(1)
   { ... }
   else
   { ... }
   为一个语义块

   stmt支持语义块嵌套
注：遇到分号会返回，所以嵌套时，若以{为tk调用stmt，它会多次调用stmt，
     确保解析完毕整个语义块。
	 所以：stmt对分号处理： 当前语义块是单个语句时，stmt遇到分号返回；当前语义块是{a; b;}时，
	 stmt自己嵌套调用，仅当遇到}才返回。
*/
void stmt()
{
	int *a, *b;
	if(tk == If)
	{
		next();
		match('(');
		next();
		expr(Assign);
		match(')');
		next();
		*++e = BZ;
		b = ++e; // 记录BZ参数的地址，稍后赋值
        stmt();  // stmt 最终可能调用expr, expr返回时，会将tk已指向下一个Token, 这里不用next()
		if(tk == Else)
		{
			*b = (int)(e+3); // e[1] == jmp, e[2] == jmp addr; e[3] = else开始指令的地址
			*++e = JMP; // if(1) 执行完毕，跳转到else结束处
			b = ++e;    // if(1) 执行完毕跳转的目的地
			next();
			stmt(); // 解析else内的语句
		}
		*b=(int)(e+1); // 下一个语句
	}
	else if(tk == While) // while被解析成2个语句块：甲（条件判断，结尾BZ）和乙（循环体，结尾JMP）
	{
		next();
		a = e + 1; // 下一个指令，甲的头部，即乙的结尾跳转目的地
        match('(');
		next();
		expr(Assign);
		match(')');
		next();
		*++e = BZ;  // 甲的尾部
        b = ++e;    // while结束后的地址
		stmt();     // 乙
		*++e = JMP;
		*++e = (int)a;
		*b = (int)(e+1);
	}
	else if(tk == Return)
	{
		next();
		if(tk != ';')
			expr(Assign);
		*++e = LEV;
		match(';');
		next();
	}
	else if(tk == '{') // 大括号嵌套
	{
		next();
		while(tk != '}') // 反复嵌套
			stmt();
		next();
	}
	else if(tk == ';')
	{
		next();
	}
	else // 嵌套顶端
	{
		expr(Assign);
		match(';');
		next();
	}
	return;
}

bool prepare()
{
	int i;
	memset(table, 0, sizeof(table));
	memset(text, 0, sizeof(text));
	memset(bss, 0, sizeof(bss));
	le = e = text;
	data = bss;
	p = "char int if else while return "
        "printf exit void main";
	i = Char;
	// 保留字放入talbe，其tk范围为Char - Return
	while(i<=Return)
	{  next(); table[id].tk = i++; }
	// 系统调用放入table，其val值从PRT(22)开始增加
	i = PRT;
	while(i<=EXT)
	{
		next();
		table[id].kind = Sys;
		table[id].type = INT;
		table[id].val = i++;
	}
	next();
	table[id].tk = Char; // "void"
	next();
	idmain = id; // "main"
	line = 1; col = 0;
	return true;
}

void symHide(int id, int kind, int type, int val)
{
	table[id].hkind = table[id].kind;
	table[id].kind = kind;
	table[id].htype = table[id].type;
	table[id].type = type;
	table[id].hval = table[id].val;
	table[id].val = val;
}

void symRestore()
{
	int i = 0;
	while(table[i].tk)
	{
		if(table[i].kind == Loc)
		{
			table[i].kind = table[i].hkind;
	        table[i].type = table[i].htype;
	        table[i].val = table[i].hval;
		}
		i++;
	}
}

// 解析C语言字符src
bool parse(char* src)
{
	int bt; // base type (像int* foo(), 其base type为int)
	int ty, i;
	prepare();
	lp = p = src;
	next();
	while(tk) // 逐个函数原型解析
	{
		if(tk == Int) bt = INT;
		else if(tk == Char) bt = CHAR;
		else Wrong;
		next();
		while(tk!=';' && tk!='}') // 函数体
		{
			ty = bt;
			while(tk==Mul) { next(); ty += PTR; };
			check(tk == Id && !table[id].kind);
			next();
			table[id].type = ty;
			match('(');
			table[id].kind = Fun;
			table[id].val = (int)(e+1); // 函数入口地址（即指令ENT的地址）
			next();
			i = 0;
			while(tk != ')') // 逐个解析函数参数
			{
				if(tk == Int) { ty = INT; }
				else if(tk == Char) { ty = Char; }
				else Wrong;
                next();
				while(tk == Mul) { next(); ty += PTR; }
				match(Id);
				check(table[id].kind != Loc);
				symHide(id, Loc, ty, i++);  // 处理函数参数与全局标识符重名
				next();
				if(tk == ',')
					next();
			}
			next();
			match('{'); // 进入函数体
			loc = ++i;
			next();
			// printEnv();
			while(tk==Int || tk == Char) // 逐行解析变量定义语句
			{
				bt = (tk==Int) ? INT : CHAR;
				next();
				while(tk != ';')
				{
					ty=bt;
					while(tk==Mul) { next(); ty += PTR; }
					match(Id);
					check(table[id].kind != Loc); // 不和函数参数或其他局部变量重名
					symHide(id, Loc, ty, ++i);
					next();
					if(tk == ',')
						next();
				}
				next();
			}
			*++e = ENT;      // 开始函数执行语句前保护现场
			*++e = i - loc;  // 局部变量个数
			while(tk != '}') // 逐个语义块解析
				stmt();
			*++e = LEV; // 函数结束，恢复现场
			symRestore();
		} // 函数体
		next();
	} // 逐个函数原型解析
	return true;
}

/* 虚拟机运行函数，指令区 text, 数据区 bss, 栈 stack
   二元运算指令共性：左操作数地址在栈顶，右操作数在A，结果存在A，运算的附属操作是弹栈，因为该操作数已经用掉不再需要
   这种共性方便expr()函数递归实现
*/
bool run(int* startaddr)
{
	int a, *pc, *sp, *bp; // 虚拟机寄存器
	int op, *t, cntr = 0; // 辅助变量
	memset(stack, 0, sizeof(stack));
	bp = sp = stack + PoolLen;
	*--sp = EXT;          // main函数返回后调用exit
	t = sp;
	*--sp = (int)t;  // 便于最后一条指令LEV 找到执行EXT
	pc = startaddr;
	while(++cntr)
	{
		op = *pc++;
		switch(op)
		{
		case ENT: { *--sp=(int)bp; bp=sp; sp-=*pc++;        } break;
		case LEV: { sp=bp; bp=(int*)*sp++; pc=(int*)*sp++;  } break;
		case JSR: { *--sp=(int)(pc+1); pc=(int*)*pc;        } break;
		case ADJ: { sp += *pc++;                            } break;
		case JMP: { pc = (int*)*pc;                         } break;
		case BZ:  { pc = (a) ? pc+1 : (int*)*pc;            } break;
		case BNZ: { pc = (a) ? (int*)*pc : pc+1;            } break;
		case LEA: { a = (int)(bp + *pc++);                  } break;
		case IMM: { a = *pc++;                              } break;
		case PSH: { *--sp = a;                              } break;
		case LI:  { a = *(int*)a;                           } break;
		case LC:  { a= *(char*)a;                           } break;
		case SI:  { *(int*)*sp++ = a;                       } break; // store int
		case SC:  { *(char*)*sp++ = a;                      } break;

		case EQ:  { a = *sp++ == a;                         } break;
		case NE:  { a = *sp++ != a;                         } break;
		case GT:  { a = *sp++ > a;                          } break;
		case ADD: { a = *sp++ + a;                          } break;
		case SUB: { a = *sp++ - a;                          } break;
		case MUL: { a = *sp++ * a;                          } break;
		case DIV: { a = *sp++ / a;                          } break;

		case PRT: { t=sp+pc[1]; a=printf((char*)t[-1],t[-2],t[-3],t[-4],t[-5], t[-6]);  } break;
		case EXT: { printf("exit(), sp:%d, cycle:%d", *sp, cntr);  return true;         } break;
		default: return Panic("cntr:%d \n", cntr);
		}
	}
	return Wrong;
}

bool panic(const char* file, const char* func, int c5line, int row, int col, const char* format, ...)
{
	va_list ap; // 变参队列
	int r;
	printf("Panic %s %s() c5Line:%d, row:%d, col:%d \n", file, func, c5line, row, col);
	printf("------- p:%s ------ \n", p);
	va_start(ap, format);    // 使得变参指向参数format后第一个参数
	r = vprintf(format, ap);
	va_end(ap); // 关闭ap
	exit(1);
	return false;
};

void _check(bool flag, const char* file, const char* func, int c5line, int row, int col)
{
	if(flag)
		return;
	printf("check fail. %s %s() c5Line:%d, row:%d, col:%d \n", file, func, c5line, row, col);
	printf("------- p:%s ------ \n", p);
	exit(2);
}

void printEnv()
{
	char* q;
	int i=0;
	printf("Table: ");
	while(table[i].tk)
	{
		q = (char*)table[i].name;
		while(isAlpha((int)*q) || isNumber((int)*q))
			printf("%c", *q++);
		printf(" ");
		i++;
	}
	printf("\n");
}

