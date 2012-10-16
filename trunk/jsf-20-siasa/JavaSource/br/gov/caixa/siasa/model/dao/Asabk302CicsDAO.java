package br.gov.caixa.siasa.model.dao;

import br.gov.caixa.siasa.model.dto.CobolBook;

public final class Asabk302CicsDAO extends MockCicsDAO implements IDao {

	public Asabk302CicsDAO(String string) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public CobolBook execute(CobolBook cb) {
		// TODO Auto-generated method stub
		StringBuilder builder = new StringBuilder("000");
		builder.append(pic(FILLER,114));
		builder.append(pic(40000L,15));
		builder.append(pic(20000L,15));
		builder.append(pic(FILLER,3618));
		builder.append('0');
		builder.append(pic("Operação realizada com sucesso.",80));
		builder.append("0000");
		cb.toCICS();
		cb.fromCICS(builder.toString());
		return cb;
	}

}
