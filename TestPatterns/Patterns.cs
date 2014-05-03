using System;

namespace TestPatterns
{
	public enum Language {
		English,
		Spanish
	}
	public interface ILanguage {
		String convert(int num);
	}
	public class Spanish : ILanguage {
		public String convert (int num) {
			var ret = "";
	        switch (num) {
				case 0: ret = "zero"; break;
				case 1: ret = "uno"; break;
       			default: ret = "~~~"; break;
	        }
			return ret;
		}
	}
	public class English : ILanguage {
		public String convert (int num) {
			var ret = "";
	        switch (num) {
				case 0: ret = "zero"; break;
				case 1: ret = "one"; break;
			default: ret = "..."; break;
	        }
			return ret;
		}
	}
	public class TestPatterns {
		public static string convert(int x, Language lang) {
			return getLanguage(lang).convert(x);
		}
		public static ILanguage getLanguage(Language lang) {
	        switch (lang) {
				case Language.English: return new English() ; break;
				case Language.Spanish: return new Spanish() ; break;
			    default: throw new Exception();
	        }
		}
	}

}

