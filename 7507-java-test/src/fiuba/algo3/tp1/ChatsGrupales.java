package fiuba.algo3.tp1;

import java.util.ArrayList;
import java.util.List;

public class ChatsGrupales extends Chats {

	private List<Grupo> grupos;

	private Grupo getGrupo(String nombre) {
		for (Grupo grupo : grupos) {
			if (grupo.getNombre().equals(nombre)) {
				return grupo;
			}
		}
		return null;
	}
	
	public ChatsGrupales() {
		grupos = new ArrayList<Grupo>();
	}

	public int cantidadDeGrupos() {
		return grupos.size();
	}
	
	public void crearGrupo(String nombreGrupo, String administrador) {
		grupos.add(new Grupo(nombreGrupo, administrador));
	}

	public void agregarContactoAGrupo(String nombreContacto, String nombreGrupo) {
		getGrupo(nombreGrupo).agregarContacto(nombreContacto);
	}

	public int cantidadMiembrosEnGrupo(String nombreGrupo) {
		return getGrupo(nombreGrupo).cantidadMiembros();
	}

	public void recibirMensajeDeGrupo(String nombreGrupo, String nombreContacto, String mensaje) {
		getGrupo(nombreGrupo).recibirMensajeDe(nombreContacto, mensaje);
	}

	public int cantidadMensajesRecibidosDelGrupo(String nombreGrupo) {
		return getGrupo(nombreGrupo).cantidadTotalMensajesRecibidos();
	}

	public int cantidadMensajesEnviadosAlGrupo(String nombreGrupo) {
		return getGrupo(nombreGrupo).cantidadTotalMensajesEnviados();
	}

	@Override
	public int cantidadDeChats() {
		return grupos.size();
	}

	@Override
	public int cantidadTotalMensajesRecibidos() {
		int mensajesRecibidos = 0;
		for (Grupo grupo : grupos) {
			mensajesRecibidos += grupo.cantidadTotalMensajesRecibidos();
		}
		return mensajesRecibidos;
	}

	@Override
	public int cantidadTotalMensajesEnviados() {
		int mensajesEnviados = 0;
		for (Grupo grupo : grupos) {
			mensajesEnviados += grupo.cantidadTotalMensajesEnviados();
		}
		return mensajesEnviados;
	}

	@Override
	public boolean existeChat(String nombre) {
		return getGrupo(nombre) != null;
	}

	@Override
	public List<String> obtenerConversacionCon(String nombre) {
		return getGrupo(nombre).obtenerConversacion();
	}

	@Override
	public void enviarMensajeA(String nombre, String mensaje) {
		getGrupo(nombre).enviarMensaje(mensaje);
	}

	@Override
	public void borrarMensajes(String nombre) {
		getGrupo(nombre).borrarMensajes();
	}

	public int cantidadMensajesRecibidosDe(String nombreContacto) {
		int mensajesRecibidos = 0;
		for (Grupo grupo : grupos) {
			mensajesRecibidos += grupo.cantidadMensajesRecibidosDe(nombreContacto);
		}
		return mensajesRecibidos;		
	}


	
	
}
