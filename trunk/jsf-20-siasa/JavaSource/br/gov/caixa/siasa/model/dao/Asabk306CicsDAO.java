package br.gov.caixa.siasa.model.dao;

import br.gov.caixa.siasa.model.dto.CobolBook;

public final class Asabk306CicsDAO extends MockCicsDAO implements IDao {

	public Asabk306CicsDAO(String string) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public CobolBook execute(CobolBook cb) {
		// TODO Auto-generated method stub
		StringBuilder builder = new StringBuilder("000");
		builder.append(pic(FILLER,47));
		builder.append("003");
		for(long l=0;l<3;l++) {
			builder.append(pic(l+1L,12));
			builder.append("01/01/0001");
			builder.append("23.59.59");
			builder.append(pic(l+10L,11));
			builder.append(pic(l+20000L,15));
			builder.append(pic(l+10L,11));
			builder.append(pic(l+40000L,15));
			builder.append(pic(l+10L,11));
			builder.append(pic(l+30000L,15));
			builder.append(pic(l+90000L,15));
		}
		builder.append(pic(FILLER,3618));
		builder.append('0');
		builder.append(pic("Operação realizada com sucesso.",80));
		builder.append("0000");
		cb.toCICS();
		cb.fromCICS(builder.toString());
		return cb;
	}

}
