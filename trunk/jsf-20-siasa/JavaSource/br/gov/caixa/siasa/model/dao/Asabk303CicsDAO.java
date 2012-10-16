package br.gov.caixa.siasa.model.dao;

import br.gov.caixa.siasa.model.dto.CobolBook;

public final class Asabk303CicsDAO extends MockCicsDAO implements IDao {

	public Asabk303CicsDAO(String string) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public CobolBook execute(CobolBook cb) {
		// TODO Auto-generated method stub
		StringBuilder builder = new StringBuilder("000");
		builder.append(pic(FILLER,40));
		builder.append("003");
		for(long l=0;l<3;l++) {
			builder.append(pic(l+10L,12));
			builder.append(pic(l+9000L,9));
			builder.append("01/01/0001");
			builder.append("02/02/0002");
			builder.append("03/03/0003");
			builder.append(pic(l+20000L,15));
			builder.append("F928658");
			builder.append(pic(l+4063,4));
		}
		builder.append(pic(FILLER,2106));
		builder.append('0');
		builder.append(pic("Operação realizada com sucesso.",80));
		builder.append("0000");
		cb.toCICS();
		cb.fromCICS(builder.toString());
		return cb;
	}

}
