/*-
 * Copyright (c)2006 Takuya SHIOZAKI,
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <set>
#include <list>
#include <cstdlib>
#include <cctype>
#include <exception>

using namespace std;

#define DONT_COPY(name)				\
name(const name &);				\
void operator = (const name &)

class Exit
{
	int m_code;
public:
	Exit(int code) throw () : m_code(code) { }
	~Exit() throw () { }
	int get_code() const { return m_code; }
};

class ExitSuccess : public Exit
{
public:
	ExitSuccess() throw () : Exit(EXIT_SUCCESS) { }
	~ExitSuccess() throw () { }
};

class ExitFailure : public Exit
{
public:
	ExitFailure() throw () : Exit(EXIT_FAILURE) { }
	~ExitFailure() throw () { }
};

class NameError
{
	string m_name;
public:
	NameError(const string &name) : m_name(name) { }
	~NameError() throw () { }
	const string &get_name() const throw ()
	{
		return m_name;
	}
};

class SimpleEx
{
	string m_message;
public:
	SimpleEx(const string &str) : m_message(str) { }
	~SimpleEx() throw() { }
	const char *what() const throw() { return m_message.c_str(); }
};

const char DIRECTIVE_CHAR = '#';
const char MACRO_CHAR = '\\';
const char MACRO_DEF_CHAR = '\\';
const char MACRO_UNDEF_CHAR = '!';
const char INCLUDE_CHAR = '<';
const char COMM_CHAR = ';';
const char QUOTE_CHAR = '\"';
const char PATH_SEP = '\\';
const char SCOPE_CHAR = ':';
const char SCOPE_AUTO_ON = '+';
const char SCOPE_AUTO_OFF = '-';

template <typename Record_>
class LoopDetector
{
	DONT_COPY(LoopDetector);
public:
	typedef list<Record_> Stack;
	typedef Stack::const_iterator StackIterator;
private:
	typedef set<string> Set;
	Set m_set;
	Stack m_stack;
	bool m_freeze;
public:
	class Looped : public NameError
	{
	public:
		explicit Looped(const string &name) : NameError(name) { }
		~Looped() throw() { }
	};
	LoopDetector() : m_freeze(false) { }
	~LoopDetector() throw () { }
	bool is_loop(const string &name) const
	{
		return m_set.find(name) != m_set.end();
	}
	void push(const string &name, const Record_ &rec)
	{
		if (this->is_loop(name)) {
			this->freeze();
			throw Looped(name);
		}
		m_set.insert(name);
		m_stack.push_front(rec);
	}
	void pop()
	{
		if (m_freeze || m_stack.empty())
			return;
		m_set.erase(*m_stack.begin());
		m_stack.pop_front();
	}
	const Record_ &get_top() const
	{
		return *m_stack.begin();
	}
	void freeze()
	{
		m_freeze = true;
	}
	void unfreeze()
	{
		m_freeze = false;
	}
	const Stack &get_stack() const
	{
		return m_stack;
	}
	void clear()
	{
		Set().swap(m_set);
		Stack().swap(m_stack);
		m_freeze = false;
	}
};

class MacroStorage
{
public:
	class Record
	{
		string m_file;
		int m_line;
		string m_contents;
	public:
		Record() : m_line(0) { }
		Record(const string &file, int line, const string &contents)
			: m_file(file), m_line(line), m_contents(contents)
		{
		}
		~Record() throw () { }
		const string &get_file() const
		{
			return m_file;
		}
		int get_line() const
		{
			return m_line;
		}
		const string &get_contents() const
		{
			return m_contents;
		}
	};
private:
	typedef map<string, Record> Mapper;
	Mapper m_mapper;
public:
	class Undefined : public NameError
	{
	public:
		explicit Undefined(const string &name) : NameError(name) { }
		~Undefined() throw() { }
	};
	MacroStorage() { }
	~MacroStorage() throw () { }
	void undef(const string &name)
	{
		Mapper::iterator i;
		if ((i=m_mapper.find(name)) != m_mapper.end())
			m_mapper.erase(i);
	}
	void define(const string &name,
		    const string &file, int line, const string &contents)
	{
		m_mapper[name] = Record(file, line, contents);
	}
	void clear()
	{
		Mapper().swap(m_mapper);
	}
	const Record &query(const string &name) const
	{
		Mapper::const_iterator i = m_mapper.find(name);
		if (i == m_mapper.end())
			throw Undefined(name);
		return i->second;
	}
};

template <class Container_,
	  class Iter_=typename Container_::const_iterator>
class Region
{
public:
	typedef typename Iter_::value_type ValueType;
private:
	Iter_ m_curpos, m_end;
	void ensure_not_end_(const string &funcname) const
	{
		if (m_curpos == m_end)
			throw SimpleEx("Region::"+funcname+
				       ": internal error.");
	}
public:
	Region() { }
	Region(Container_ &container)
		: m_curpos(container.begin()), m_end(container.end())
	{
	}
	Region(Iter_ begin, Iter_ end)
		: m_curpos(begin), m_curpos(end)
	{
	}
	Region(const Region &r1, const Region &r2)
		: m_curpos(r1.curpos()), m_end(r2.curpos())
	{
	}
	Region &operator = (Container_ &container)
	{
		m_curpos = container.begin();
		m_end = container.end();
		return *this;
	}
	bool operator == (const Region &r) const
	{
		return m_curpos == r.curpos() && m_end == r.end();
	}
	~Region() throw () { }
	bool is_end() const throw ()
	{
		return m_curpos == m_end;
	}
	const Region &operator ++ ()
	{
		ensure_not_end_("++");
		++m_curpos;
		return *this;
	}
	const Region &operator -- ()
	{
		ensure_not_end_("--");
		--m_curpos;
		return *this;
	}
	Region operator ++ (int)
	{
		Region tmp = *this;
		++(*this);
		return tmp;
	}
	Region operator -- (int)
	{
		Region tmp = *this;
		--(*this);
		return tmp;
	}
	ValueType operator * () const
	{
		ensure_not_end_("operator * ()");
		return *m_curpos;
	}
	operator Container_ () const
	{
		return Container_(m_curpos, m_end);
	}
	Iter_ curpos() const
	{
		return m_curpos;
	}
	Iter_ end() const
	{
		return m_end;
	}
	int length() const
	{
		return m_end-m_curpos;
	}
};
typedef Region<const string> ConstStringRegion;

class MacroProcessor
{
private:
	MacroStorage m_storage;
	LoopDetector<string> m_loop_detector;
	bool m_auto_scope;
	string m_current_scope;
	string make_scoped_(const string &name) const
	{
		if (name[0] != SCOPE_CHAR)
			return (string() +
				SCOPE_CHAR +
				m_current_scope +
				SCOPE_CHAR +
				name);
		return name;
	}
	string make_global_(const string &name) const
	{
		if (name[0] != SCOPE_CHAR)
			return (string() +
				SCOPE_CHAR +
				SCOPE_CHAR +
				name);
		return name;
	}
public:
	typedef LoopDetector<string>::Stack Stack;
	typedef LoopDetector<string>::StackIterator StackIterator;
	class Looped : public LoopDetector<string>::Looped
	{
	public:
		Looped(const string &name)
			: LoopDetector<string>::Looped(name)
		{
		}
		~Looped() throw () { }
	};
	typedef MacroStorage::Undefined Undefined;
	typedef MacroStorage::Record Record;
	MacroProcessor() : m_auto_scope(false) { }
	~MacroProcessor() throw() { }
	class Locker;
	friend class Locker;
	class Locker
	{
		DONT_COPY(Locker);
	private:
		MacroProcessor &m_mp;
		const MacroStorage::Record *m_result;
	public:
		Locker(MacroProcessor &mp, const string &name)
			: m_mp(mp),
			  m_result(NULL)
		{
			try {
				string n1, n2;
				try {
					n1 = m_mp.make_scoped_(name);
					m_result = &m_mp.m_storage.query(n1);
					m_mp.m_loop_detector.push(n1, n1);
				}
				catch (Undefined &) {
					try {
						n2 = m_mp.make_global_(name);
						m_result =
						    &m_mp.m_storage.query(n2);
						m_mp.m_loop_detector.push(n2,
									  n2);
					}
					catch (Undefined &) {
						throw Undefined(name);
					}
				}
			}
			catch (LoopDetector<string>::Looped &ex) {
				throw Looped(ex.get_name());
			}
		}
		~Locker() throw()
		{
			// if the constructor throws some exception,
			// the destructor never be called.
			m_mp.m_loop_detector.pop();
		}
		const string &query() const
		{
			if (!m_result)
				throw SimpleEx("internal error.");
			return m_result->get_contents();
		}
	};
	void set_scope(const string &s)
	{
		m_current_scope = s;
	}
	void set_auto_scope_mode(bool mode)
	{
		m_auto_scope = mode;
	}
	bool is_auto_scope() const
	{
		return m_auto_scope;
	}
	void undef(const string &name)
	{
		m_storage.undef(make_scoped_(name));
	}
	void define(const string &name,
		    const string &file, int line, const string &contents)
	{
		m_storage.define(make_scoped_(name), file, line, contents);
	}
	void clear()
	{
		m_storage.clear();
	}
	const Stack &get_stack() const
	{
		return m_loop_detector.get_stack();
	}
	const Record &query(const string &name) const
	{
		return m_storage.query(make_scoped_(name));
	}
};

class PathList
{
	typedef list<string> List;
	List m_list;
public:
	class CannotOpen : public NameError
	{
	public:
		CannotOpen(const string &name) : NameError(name) { }
		~CannotOpen() throw () { }
	};
	PathList()
	{
		m_list.push_front(".");
	}
	~PathList() throw() { }
	void push(const string &path)
	{
		if (path.empty())
			return;
		string::const_iterator tmp = path.end();
		if (*--tmp != PATH_SEP)
			tmp = path.end();
		m_list.push_front(string(path.begin(), tmp));
	}
	void open(ifstream &ifs, const string &name)
	{
		if (name.empty())
			throw CannotOpen("");
		if (*name.begin() == PATH_SEP) {
			ifs.close();
			ifs.clear();
			ifs.open(name.c_str());
			if (!ifs.is_open())
				throw CannotOpen(name);
			return;
		}
		for (List::const_iterator i=m_list.begin();
		     i!=m_list.end();
		     ++i) {
			ifs.close();
			ifs.clear();
			ifs.open((*i+PATH_SEP+name).c_str());
			if (ifs.is_open())
				return;
		}
		throw CannotOpen(name);
	}
};

class IncludeProcessor
{
public:
	class Record
	{
		string m_file;
		string m_base_file;
		int m_base_line;
	public:
		Record() : m_base_line(0) { }
		Record(const string &file,
		       const string &base_file, int base_line)
			: m_file(file),
			  m_base_file(base_file), m_base_line(base_line)
		{
		}
		~Record() throw () { }
		operator const string & () const
		{
			return m_file;
		}
		const string &get_name() const
		{
			return m_file;
		}
		const string &get_file() const
		{
			return m_file;
		}
		const string &get_base_file() const
		{
			return m_base_file;
		}
		int get_base_line() const
		{
			return m_base_line;
		}
	};
private:
	LoopDetector<Record> m_loop_detector;
	PathList m_path_list;
public:
	typedef LoopDetector<Record>::Stack Stack;
	typedef LoopDetector<Record>::StackIterator StackIterator;
	typedef PathList::CannotOpen CannotOpen;
	IncludeProcessor() { }
	~IncludeProcessor() throw() { }
	class Looped : public LoopDetector<Record>::Looped
	{
	public:
		Looped(const string &name)
			: LoopDetector<Record>::Looped(name)
		{
		}
		~Looped() throw () { }
	};
	class Locker;
	friend class Locker;
	class Locker
	{
		DONT_COPY(Locker);
	private:
		IncludeProcessor &m_ip;
	public:
		Locker(IncludeProcessor &ip, const string &name,
		       const string &base_file="", int base_line=0) : m_ip(ip)
		{
			try {
				m_ip.m_loop_detector.push(name,
							  Record(name,
								 base_file,
								 base_line));
			}
			catch (LoopDetector<Record>::Looped &ex) {
				throw Looped(ex.get_name());
			}
		}
		~Locker() throw()
		{
			// if the constructor throws some exception,
			// the destructor never be called.
			m_ip.m_loop_detector.pop();
		}
	};
	void push(const string &path)
	{
		m_path_list.push(path);
	}
	void open(ifstream &ifs, const string &name)
	{
		m_path_list.open(ifs, name);
	}
	const Stack &get_stack() const
	{
		return m_loop_detector.get_stack();
	}
};

class FileContext
{
	MacroProcessor &m_macro_processor;
	IncludeProcessor &m_include_processor;
	ostream &m_logger;
	string m_input_file_name;
	istream &m_input_stream;
	string m_output_file_name;
	ostream &m_output_stream;
	int m_line_number;
//
	FileContext(MacroProcessor &macro_processor,
		    IncludeProcessor &include_processor,
		    ostream &logger,
		    const string &input_file_name,
		    istream &input_stream,
		    const string &output_file_name,
		    ostream &output_stream)
		: m_macro_processor(macro_processor),
		  m_include_processor(include_processor),
		  m_logger(logger),
		  m_input_file_name(input_file_name),
		  m_input_stream(input_stream),
		  m_output_file_name(output_file_name),
		  m_output_stream(output_stream),
		  m_line_number(0)
	{
	}
	~FileContext() throw () { }
	void do_define_macro_(ConstStringRegion) const;
	void do_undef_macro_(ConstStringRegion) const;
	void do_include_(ConstStringRegion) const;
	void do_set_scope_(ConstStringRegion) const;
	static bool skip_macro_directive_chars_(ConstStringRegion *input);
	bool directive_(ConstStringRegion,
			void (FileContext::*)(ConstStringRegion) const,
			const char) const;
	bool define_macro_(const ConstStringRegion &input) const
	{
		return directive_(input, do_define_macro_, MACRO_DEF_CHAR);
	}
	bool undef_macro_(const ConstStringRegion &input) const
	{
		return directive_(input, do_undef_macro_, MACRO_UNDEF_CHAR);
	}
	bool set_scope_(const ConstStringRegion &input) const
	{
		return directive_(input, do_set_scope_, SCOPE_CHAR);
	}
	bool include_(const ConstStringRegion &input) const
	{
		return directive_(input, do_include_, INCLUDE_CHAR);
	}
	string expand_(ConstStringRegion) const;
	void process_();
public:
	MacroProcessor &get_macro_processor() const
	{
		return m_macro_processor;
	}
	void put_message(const string &fac, const string &msg) const
	{
		m_logger << fac << " at line " << m_line_number << " in "
			 << m_input_file_name << ": " << msg << std::endl;
	}
	static void process(MacroProcessor &macro_processor,
			    IncludeProcessor &include_processor,
			    ostream &logger,
			    string input_name, istream &input_stream,
			    string output_name, ostream &output_stream)
	{
		FileContext ctx(macro_processor, include_processor, logger,
				input_name, input_stream,
				output_name, output_stream);
		ctx.process_();
	}
};

void
skip_ws(ConstStringRegion *pr)
{
	ConstStringRegion &r=*pr;
	for (; !r.is_end(); ++r)
		if (!isspace(*r))
			break;
}

string
get_macro_name(ConstStringRegion *pr, bool *rscoped = NULL)
{
	ConstStringRegion &r = *pr;
	const ConstStringRegion saved = r;
	bool scoped = false, scoped_done = false, body=false;
	string ret;

	for (; !r.is_end(); ++r) {
		if (r == saved && *r == SCOPE_CHAR)
			scoped = true;
		else if (scoped && !scoped_done) {
			if (*r == SCOPE_CHAR)
				scoped_done = true;
			else if (!(isalnum(*r) || *r == '_'))
				break;
		} else if (!(isupper(*r) || *r == '_' || isdigit(*r)))
			break;
		else if (!scoped || scoped_done)
			body = true;
	}
	if (scoped && !scoped_done)
		throw SimpleEx("ill-formed macro scope.");
	if (!body)
		throw SimpleEx("cannot get macro name.");
	if (rscoped)
		*rscoped = scoped;
	return ConstStringRegion(saved, r);
}

string
get_scope_name(ConstStringRegion *pr)
{
	ConstStringRegion &r = *pr;
	const ConstStringRegion saved = r;

	for (; !r.is_end(); ++r) {
		if (!(isalnum(*r) || *r == '_'))
			break;
	}

	return ConstStringRegion(saved, r);
}

string
get_string(ConstStringRegion input, bool *quoted = NULL)
{
	ConstStringRegion begin, end;
	char quote_char = 0;

	if (quoted)
		*quoted = false;
	skip_ws(&input);

	if (input.is_end())
		return input;

	begin = end = input;
	for (; !input.is_end(); ++input) {
		if (*input == QUOTE_CHAR) {
			if (begin == input) {
				quote_char = *input;
				++begin;
				continue;
			} else {
				if (!quote_char)
					throw SimpleEx("unexpected quote "
						       "character.");
				quote_char = 0;
				++input;
				skip_ws(&input);
				if (quoted)
					*quoted = true;
				if (!input.is_end() &&
				    *input != COMM_CHAR)
					throw SimpleEx("unexpected character "
						       "after quotation.");
				break;
			}
		}
		if (*input == COMM_CHAR)
			break;
		if (quote_char || !isspace(*input))
			end = input;
	}
	if (quote_char)
		throw SimpleEx("unclosed quotation.");

	return string(ConstStringRegion(begin, ++end));
}

void
FileContext::do_define_macro_(ConstStringRegion input) const
{
	string name;
	string additional = " "; // default: add one space character.
	string str;
	bool quoted;

	name = get_macro_name(&input);
	str = get_string(input, &quoted);
	if (str.empty() && !quoted) {
//		m_macro_processor.undef(name);
		m_macro_processor.define(name,
					 m_input_file_name, m_line_number, "");
	} else {
		if (quoted)
			additional = "";
		m_macro_processor.define(name,
					 m_input_file_name, m_line_number,
					 str + additional);
	}
}

void
FileContext::do_undef_macro_(ConstStringRegion input) const
{
	string name;

	name = get_macro_name(&input);
	m_macro_processor.undef(name);
}

void
FileContext::do_include_(ConstStringRegion input) const
{
	ifstream ifs;
	string file;

	file = get_string(input);
	m_include_processor.open(ifs, file);

	IncludeProcessor::Locker locker(m_include_processor, file,
					m_input_file_name, m_line_number);
	FileContext::process(m_macro_processor, m_include_processor, m_logger,
			     file, ifs, m_output_file_name, m_output_stream);
}

void
FileContext::do_set_scope_(ConstStringRegion input) const
{
	string name;

	if (!input.is_end()) {
		switch (*input) {
		case SCOPE_AUTO_ON:
			m_macro_processor.set_auto_scope_mode(true);
			return;
		case SCOPE_AUTO_OFF:
			m_macro_processor.set_auto_scope_mode(false);
			return;
		}
	}
	name = get_scope_name(&input);
	m_macro_processor.set_scope(name);
}

bool
FileContext::skip_macro_directive_chars_(ConstStringRegion *input)
{
	skip_ws(input);
	return input->length() > 1 && *(*input)++ == DIRECTIVE_CHAR;
}

bool
FileContext::directive_(ConstStringRegion input,
			void (FileContext::*dofunc_)(ConstStringRegion) const,
			const char dirchar) const
{

	if (skip_macro_directive_chars_(&input) && *input == dirchar) {
		++input;
		(this->*dofunc_)(input);
		return true;
	}
	return false;
}

string
FileContext::expand_(ConstStringRegion input) const
{
	bool dbcs = false;
	bool macro_char = false;
	bool enter_out = false;
	string out;
	ConstStringRegion begin, saved=input;

	begin = input;
	while (!input.is_end()) {
		if (dbcs) {
			dbcs = false;
			goto next;
		}
		if ((*input & 0x80) != 0) {
			dbcs = true;
			goto next;
		}
		if (macro_char) {
			macro_char = false;
			if (*input == MACRO_CHAR) {
				ConstStringRegion tmp = input;
				out += string(ConstStringRegion(begin, --tmp));
				out += '\\';
				enter_out = true;
				begin = ++input;
				continue;
			}
			ConstStringRegion tmp = input;
			out += string(ConstStringRegion(begin, --tmp));
			enter_out = true;
			try {
				MacroProcessor::Locker locker(
					m_macro_processor,
					get_macro_name(&input));
				out += this->expand_(locker.query());
			}
			catch (MacroProcessor::Undefined &ex) {
#if 1
				throw SimpleEx(string("macro \"")+
					       MACRO_CHAR+ex.get_name()+
					       "\" is not defined.");
#else
				this->put_message(
					"warning",
					string("macro \"")+
					MACRO_CHAR+ex.get_name()+
					"\" is not defined.");
#endif
			}
			if (!input.is_end() && isspace(*input))
				++input;
			begin = input;
			continue;
		}
		if (*input == MACRO_CHAR)
			macro_char = true;
next:
		++input;
	}
	if (enter_out) {
		out += string(begin);
		return out;
	}
	return saved;
}

void
FileContext::process_()
{
	try {
		while (m_input_stream.good()) {
			string input;

			getline(m_input_stream, input);
			m_line_number++;
			if (this->define_macro_(input))
				m_output_stream << endl;
			else if (this->undef_macro_(input))
				m_output_stream << endl;
			else if (this->set_scope_(input))
				m_output_stream << endl;
			else if (this->include_(input))
				;
			else {
				if (m_macro_processor.is_auto_scope() &&
				    input.size() > 0 &&
				    isalpha((int)(unsigned char)input[0])) {
					m_macro_processor.set_scope(
						string(1, input[0]));
				}
				m_output_stream << this->expand_(input)
						<< endl;
			}
			if (m_output_stream.bad())
				throw SimpleEx("cannot write to file \"" +
					       m_output_file_name + "\"");
		}
	}
	catch (SimpleEx &ex) {
		this->put_message("error", ex.what());
		throw ExitFailure();
	}
	catch (MacroProcessor::Looped &ex) {
		this->put_message("error",
				  string("macro ")+MACRO_CHAR+ex.get_name()+
				  " is looped:");
		typedef MacroProcessor::Stack Stack;
		typedef MacroProcessor::StackIterator StackIterator;
		typedef MacroProcessor::Record Record;
		const Stack &stack = m_macro_processor.get_stack();
		for (StackIterator i = stack.begin(); i != stack.end(); ++i) {
			const Record &rec = m_macro_processor.query(*i);
			m_logger << "\t" << MACRO_CHAR << *i
				 << " defined at line " << rec.get_line()
				 << " in " << rec.get_file()
				 << endl;
		}
		throw ExitFailure();
	}
	catch (IncludeProcessor::Looped &ex) {
		this->put_message("error",
				  "including file \""+ex.get_name()+
				  "\" is looped:");
		typedef IncludeProcessor::Stack Stack;
		typedef IncludeProcessor::StackIterator StackIterator;
		const Stack &stack = m_include_processor.get_stack();
		for (StackIterator i = stack.begin(); i != stack.end(); ++i) {
			if (i->get_base_file().empty())
				break;
			m_logger << "\t\"" << i->get_file()
				 << "\" include at line " << i->get_base_line()
				 << " in \"" << i->get_base_file() << "\""
				 << endl;
		}
		throw ExitFailure();
	}
	catch (PathList::CannotOpen &ex) {
		this->put_message("error",
				  "file \""+ex.get_name()+
				  "\" cannot be opened.");
		throw ExitFailure();
	}
}

bool f_banner = true;

void
banner()
{
	if (f_banner)
		cerr << "macro for mck (c)2006 T.SHIOZAKI." << endl;
	f_banner = false;
}

void
usage()
{
	banner();
	cerr << "usage: mckmacro [-q] [-o outfile] [infile]" << endl;
	exit(EXIT_FAILURE);
}

void
warnx(const char *fmt)
{
	banner();
	cerr << fmt << endl;
}

void
errx(int excode, const char *fmt)
{
	warnx(fmt);
	exit(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
	MacroProcessor macro_processor;
	IncludeProcessor include_processor;
	ifstream ifs;
	ofstream ofs;
	istream *is = &cin;
	ostream *os = &cout;
	string ifile;
	string ofile;
	bool done = false;

	argv++;
	argc--;
	while (!done && argc > 0 && *argv[0] == '-') {
		switch (argv[0][1]) {
		case '-':
			done = true;
			break;
		case 'q':
			f_banner = false;
			break;
		case 'o':
			if (argc < 2)
				usage();
			if (ofile.size() > 0)
				errx(EXIT_FAILURE,
				     "error: duplicate -o option.");
			argv++;
			argc--;
			ofile = argv[0];
			break;
		default:
			warnx((string("error: unknown option -")+
			       string(1, argv[0][1])+".").c_str());
			usage();
		}
		argv++;
		argc--;
	}
	if (argc > 1) {
		warnx("error: too many input files.");
		usage();
	} else if (argc == 1)
		ifile = argv[0];

	if (ifile.size() > 0) {
		ifs.open(ifile.c_str());
		if (ifs.fail())
			errx(EXIT_FAILURE,
			     (string("error: cannot open file \"")+
			      ifile+"\".").c_str());
		is = &ifs;
	} else {
		ifile = "<stdin>";
	}
	if (ofile.size() > 0) {
		ofs.open(ofile.c_str());
		if (ofs.fail())
			errx(EXIT_FAILURE,
			     (string("error: cannot open file \"")+
			      ofile+"\".").c_str());
		os = &ofs;
	} else {
		ofile = "<stdout>";
	}
	banner();
	try {
		IncludeProcessor::Locker locker(include_processor, ifile);
		FileContext::process(macro_processor, include_processor, cerr,
				     ifile, *is, ofile, *os);
	} catch (Exit &ex) {
		return ex.get_code();
	}

	return EXIT_SUCCESS;
}
