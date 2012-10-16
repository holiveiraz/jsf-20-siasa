package br.gov.caixa.siasa.model.dao;

public final class CicsDaoFactory {

	public static IDao getDao(String coPrograma) {
		if(coPrograma.substring(0,3) == "ASA") return new CicsDAO(coPrograma);
		else if(coPrograma == "MOCKPO301") return new Asabk301CicsDAO("ASAPO301");
		else if(coPrograma == "MOCKPO302") return new Asabk302CicsDAO("ASAPO302");
		else if(coPrograma == "MOCKPO303") return new Asabk303CicsDAO("ASAPO303");
		else if(coPrograma == "MOCKPO304") return new Asabk304CicsDAO("ASAPO304");
		else if(coPrograma == "MOCKPO305") return new Asabk305CicsDAO("ASAPO305");
		else if(coPrograma == "MOCKPO306") return new Asabk306CicsDAO("ASAPO306");
		else if(coPrograma == "MOCKPO307") return new Asabk307CicsDAO("ASAPO307");
		else if(coPrograma == "MOCKPO308") return new Asabk308CicsDAO("ASAPO308");
		return null;
	}

}
